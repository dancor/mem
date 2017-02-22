{- Requirements:
 -  - record for every q-display: q, display time, correct or not, response time
 -  - allow a specified user (when studying alone vs with ecca, etc)
 -  - correct or not:
 -    - correct
 -    - wrong
 -    - cancelled (saw answer, something weird happened)
 -    - error (program ^C, etc)
 -}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad.Error
import Control.Monad.Random
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Console.GetOpt
import System.Console.Haskeline
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Random
import System.Time
import qualified Data.Map as M
import qualified System.Process as SP

type Qna = (Que, Ans)

type Que = String

type Ans = String

type QHist = (ClockTime, Bool)

-- select a question possibly based on history in some way
data QSelect = QSRandom | QSLastCorrectDeltaTimes Float

-- select a question when nothing is suggested by history
data QNewSelect = QNSRandom | QNSInitial

-- things that get stringified into the database in some way
class DbStr a where
  dbStr :: a -> String

instance DbStr QSelect where
  dbStr QSRandom = "random"
  dbStr (QSLastCorrectDeltaTimes i) = "lastCorrectDeltaTimes " ++ show i

instance DbStr QNewSelect where
  dbStr QNSRandom = "random"
  dbStr QNSInitial = "initial"

data Flag = FUser String | FQSelect String | FQNewSelect String | 
  FMaxLine String | FMinTimeSinceLastSeen String

type OptVals = (String, QSelect, QNewSelect, Maybe Int, Int)

putStrLnF :: String -> IO ()
putStrLnF s = putStrLn s >> hFlush stdout

askQ :: Qna -> Maybe String -> InputT IO (ClockTime, ClockTime, Bool, Bool)
askQ (q, a) comm = do
  liftIO $ putStrLnF q
  tS <- liftIO getClockTime
  r <- fromMaybe "" <$> getInputLine ""
  tA <- liftIO getClockTime
  {- check mode should be an option (but not default?)
  if r == a
    then do
      putStrLnF "Yes"
      return True
    else do
      putStrLnF "No"
      putStrLnF a
      return False
  -}
  liftIO . when (r == a) $ putStrLn "matches"
  liftIO . putStrLnF $ "\n" ++ a
  flip (maybe (return ())) comm $ \ c -> do
    (_, pO, _, _) <- liftIO . SP.runInteractiveCommand $ c ++ " " ++ q
    pC <- liftIO $ hGetContents pO
    liftIO $ putStr pC
  liftIO $ putStrLnF "\nCorrect (enter for yes, q for yes and quit, else for no)?"
  c <- fromMaybe "" <$> getInputLine ""
  return (tS, tA, c == "" || c == "q", c == "q")

getConnection :: IO Connection
getConnection = do
  home <- getHomeDirectory
  let sqlFile = home </> ".config" </> "mem"
  dbExisted <- doesFileExist sqlFile
  createDirectoryIfMissing True (home </> ".config")
  conn <- handleSqlError $ connectSqlite3 sqlFile
  unless dbExisted . withTransaction conn $ \c -> run c (
    "CREATE TABLE ask_log (" ++
    "question VARCHAR(255), " ++
    "got_correct BOOL, " ++
    "asked_time INT, " ++
    "asked_time_nanosec INT, " ++
    "answered_time INT, " ++
    "answered_time_nanosec INT, " ++
    "question_select_method VARCHAR(64), " ++
    "question_sub_select_method VARCHAR(64), " ++
    "question_set VARCHAR(255), " ++
    "answerer VARCHAR(64))") [] >> return ()
  return conn

getQSetHistory :: String -> IO [(Que, QHist)]
getQSetHistory qSet = do
  conn <- getConnection
  ret <- withTransaction conn $ \ c -> quickQuery c
    "SELECT question, got_correct, asked_time, asked_time_nanosec, \
    \answered_time, answered_time_nanosec, question_select_method FROM \
    \ask_log WHERE question_set = ?" [toSql qSet]
  -- why does hdbc give integer columns as strings?!
  -- convert nanoseconds to picoseconds for TOD
  return . flip map ret $ \ row -> (fromSql $ row!!0, (
      --TOD (read . fromSql $ row!!2) ((read . fromSql $ row!!3) * 1000),
      TOD (read . fromSql $ row!!4) ((read . fromSql $ row!!5) * 1000),
      fromSql $ row!!1
      )
    )

timePDiff :: ClockTime -> ClockTime -> Integer
timePDiff (TOD xs xp) (TOD ys yp) = (xs - ys) * pSecInSec + xp - yp

mapMultiPartition :: (Ord k) => [v -> Bool] -> M.Map k v -> [M.Map k v]
mapMultiPartition (p:ps) m = t : mapMultiPartition ps f where
  (t, f) = M.partition p m
mapMultiPartition _ m = [m]

doQSelect :: Int -> QSelect -> QNewSelect -> (String, [Qna]) -> IO (Either String Qna)
-- should QSRandom respect minTimeSinceLastSeen as well?
doQSelect _ QSRandom _ (_, qnas) = Right <$> evalRandIO (randChoice qnas)
doQSelect minTimeSinceLastSeen (QSLastCorrectDeltaTimes lcdt) qNewSelect (qSet, qnas) = 
  liftM2 f (getQSetHistory qSet) getClockTime >>= evalRandIO where
  f qSetHistory now = 
    head . (\ nev -> map (Right . second (snd . snd)) (qLastTwoRightReady ++
        sortBy (flip $ comparing snd) (M.toList qLastOneRight) ++
        sortBy (flip $ comparing snd) (M.toList qEverAsked) ++ 
        nev) ++ 
      [partyOver]) <$>
    qnsF (M.toList qNeverAsked)
    where
    qLastTwoRightReady = map fst . sortBy (flip $ comparing snd) .
      filter ((> 0) . snd) . 
      map (\ x -> (x, lastTwoTimesDiff now lcdt $ snd x)) $
      M.toList qLastTwoRight
    [qLastTwoRight, qLastOneRight, qEverAsked, qNeverAsked] = 
      mapMultiPartition [lastTwoRight, lastOneRight, everAsked] okQHists
    okQHists :: M.Map Que ([QHist], (Int, Ans))
    okQHists = 
      M.filter (\ (h, _) -> and $ map 
        ((> fromIntegral minTimeSinceLastSeen * pSecInSec) . timePDiff now . 
        fst) h) .
      M.map (first $ sortBy (flip compare)) $
      M.differenceWith (\ (_, (n, a)) h -> Just (h, (n, a)))
        (M.fromList $ zipWith (\ n (q, a) -> (q, ([], (n, a)))) [1 ..] qnas)
        (M.fromListWith (++) $ map (second (:[])) qSetHistory)
    qnsF :: [(Que, ([QHist], (Int, Ans)))] -> 
      Rand StdGen [(Que, ([QHist], (Int, Ans)))]
    qnsF = case qNewSelect of
      QNSInitial -> return . sortBy (comparing $ fst . snd . snd)
      QNSRandom -> randShuffle
    partyOver = Left $
      "All questions correct or seen too recently!  Mem something else for " ++
      "a bit.."

lastTwoTimesDiff :: ClockTime -> Float -> ([QHist], a) -> Float
lastTwoTimesDiff now lcdt ((t, _):(t2, _):_, _) =
  fromIntegral (timePDiff now t) - lcdt * fromIntegral (timePDiff t t2)

lastTwoRight :: ([QHist], a) -> Bool
lastTwoRight ((_, t):(_, t2):_, _) = t && t2
lastTwoRight _ = False

lastOneRight :: ([QHist], a) -> Bool
lastOneRight ((_, t):_, _) = t
lastOneRight _ = False

everAsked :: ([QHist], a) -> Bool
everAsked q = length (fst q) > 0

recordQ :: String -> QNewSelect -> QSelect -> String -> String -> 
  (ClockTime, ClockTime, Bool) -> IO ()
recordQ answerer qSelect askMethod qSet q (TOD tSs tSp, TOD tAs tAp, b) = do
  conn <- getConnection
  ret <- withTransaction conn $ \ c -> run c 
    "INSERT INTO ask_log (question_set, question, got_correct, asked_time, \
    \asked_time_nanosec, answered_time, answered_time_nanosec, \
    \question_select_method, question_sub_select_method, answerer) VALUES \
    \(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    [
      toSql qSet, toSql q, toSql b, 
      SqlInteger tSs, SqlInteger $ tSp `div` 1000, 
      SqlInteger tAs, SqlInteger $ tAp `div` 1000, 
      toSql $ dbStr askMethod, 
      toSql $ dbStr qSelect, toSql answerer
      ]
  disconnect conn

clrScr :: IO ()
clrScr = SP.system "clear" >> return ()

pSecInSec :: Integer
pSecInSec = 1000 ^ 4

-- substitute a sublist (e.g. string replace)
subst :: Eq a => [a] -> [a] -> [a] -> [a]
subst _ _ [] = []
subst from to xs@(a:as) =
  if from `isPrefixOf` xs
    then to ++ subst from to (drop (length from) xs)
    else a : subst from to as

sublistIx :: Eq a => [a] -> [a] -> Maybe Int
sublistIx subl l = findIndex id $ map (subl `isPrefixOf`) (tails l)

randShuffle :: (MonadRandom m) => [b] -> m [b]
randShuffle l = do
  rndInts <- getRandoms
  return . map snd . sortBy (compare `on` fst) $ zip (rndInts :: [Int]) l

randChoice :: (MonadRandom m) => [b] -> m b
randChoice l = randShuffle l >>= return . head

askQs :: String -> Int -> QSelect -> QNewSelect -> (String, [Qna]) -> 
  Maybe String -> InputT IO ()
askQs answerer minTimeSinceLastSeen askMethod qSelect qqs@(qSet, qnas) comm = do
  qOrErr <- liftIO $ doQSelect minTimeSinceLastSeen askMethod qSelect qqs
  case qOrErr of
    Left e -> liftIO $ putStrLnF e
    Right q -> do
      (tS, tA, b, thenQuit) <- askQ q comm
      liftIO $ recordQ answerer qSelect askMethod qSet (fst q) (tS, tA, b)
      unless thenQuit $ do
        liftIO clrScr
        askQs answerer minTimeSinceLastSeen askMethod qSelect qqs comm

readQ :: String -> Either String (Maybe Qna)
readQ s =
  if null s || head s == '#'
    then Right Nothing
    else case sublistIx "|" s of
      Nothing -> Left "could not find | in non-empty non-comment"
      Just i ->
        let (l, r) = splitAt i s in
          Right $ Just (l, subst "<BR>" "\n" $ drop 1 r)

-- switch to parsec?  isLeft is ghetto, at least kill that..
readQs :: [String] -> Either String (String, [Qna])
readQs s = if length s >= 1 && isPrefixOf qSetPrefix (head s)
  then
    case qsMOrErr of
      Left e -> Left $ show (fromJust (findIndex isLeft qsME) + 1) ++ ": " ++ e
      Right qsM -> Right (qSet, catMaybes qsM)
  else Left "1: no question set specification found"
  where
  qSetPrefix = "# question set: "
  prefAndQSet:rest = s
  qSet = drop (length qSetPrefix) prefAndQSet
  qsME = map readQ $ rest
  qsMOrErr = sequence qsME
  isLeft (Left _) = True
  isLeft _ = False

options :: [OptDescr Flag]
options = [
  Option "u" ["user"] (ReqArg FUser "USER") "user name",
  Option "a" ["askmethod"] (ReqArg FQSelect "r|d")
    "askmethod: random or doubling",
  Option "q" ["qselect"] (ReqArg FQNewSelect "r|i")
    "qselect: random or initial",
  Option "h" ["max"] (ReqArg FMaxLine "N") "maximum line number",
  Option "t" ["min-time-since-last-seen"] (ReqArg FMinTimeSinceLastSeen "SECS")
    "don't show questions seen within this many seconds ago (default 7200)"
  ]

procOpt :: Flag -> OptVals -> OptVals
procOpt s (a, m, q, n, t) = case s of
  FUser answerer -> (answerer, m, q, n, t)
  FQSelect meth -> case meth of
    "r" -> (a, QSRandom, q, n, t)
    "d" -> (a, QSLastCorrectDeltaTimes 2, q, n, t)
    _ -> error "not a valid askmethod"
  FQNewSelect qnas -> case qnas of
    "r" -> (a, m, QNSRandom, n, t)
    "i" -> (a, m, QNSInitial, n, t)
    _ -> error "not a valid qselect"
  FMaxLine n' -> (a, m, q, Just $ read n', t)
  FMinTimeSinceLastSeen t' -> (a, m, q, n, read t')

main :: IO ()
main = runInputT (Settings noCompletion Nothing False) $ do
  args <- liftIO getArgs
  let
    header = "Usage:"
    (opts, qnaFNs) = case getOpt Permute options args of
      (o, n, []) -> (o, n)
      (_, _, errs) -> error $ concat errs ++ usageInfo header options
    (answerer, askMethod, qSelect, maxLineMby, minTimeSinceLastSeen) = foldr 
      procOpt ("", QSLastCorrectDeltaTimes 2, QNSRandom, Nothing, 7200) opts
    maxLineF = case maxLineMby of
      Nothing -> id 
      Just maxLine -> take maxLine
  forM_ qnaFNs $ \ qnaFN -> do
    qnaF <- liftIO $ openFile qnaFN ReadMode
    c <- liftIO $ hGetContents qnaF
    case readQs . maxLineF $ lines c of
      Left e -> liftIO . putStrLnF $ "error parsing " ++ qnaFN ++ ":" ++ e
      Right r -> do
        liftIO $ clrScr
        liftIO . putStrLn $ fst r
        askQs answerer minTimeSinceLastSeen askMethod qSelect r Nothing

