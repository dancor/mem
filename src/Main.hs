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
import Database.HDBC
import Database.HDBC.PostgreSQL
import FUtil
import System
import System.Console.GetOpt
import System.Console.Haskeline
import System.Environment
import System.IO
import System.Random
import System.Time
import qualified Data.Map as M
import qualified System.Process as SP

type Qna = (String, String)

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
  FMaxLine String

putStrLnF :: String -> IO ()
putStrLnF s = putStrLn s >> hFlush stdout

askQ :: Qna -> String -> InputT IO (ClockTime, ClockTime, Bool, Bool)
askQ (q, a) comm = do
  io clrScr
  io $ putStrLnF q
  tS <- io getClockTime
  r <- fromMaybe "" <$> getInputLine ""
  tA <- io getClockTime
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
  io . when (r == a) $ putStrLn "matches"
  io . putStrLnF $ "\n" ++ a
  (pI, pO, pE, pH) <- io $ SP.runInteractiveCommand (comm ++ " " ++ q)
  pC <- io $ hGetContents pO
  io $ putStr pC
  io $ putStrLnF "\nCorrect (enter for yes, q for yes and quit, else for no)?"
  c <- fromMaybe "" <$> getInputLine ""
  return (tS, tA, c == "" || c == "q", c == "q")

cPS :: IO Connection
cPS = handleSqlError $ connectPostgreSQL "dbname=memorization"

-- get all the history for a given question_set
getQSetHistory :: String -> IO [(String, (ClockTime, ClockTime, Bool))]
getQSetHistory qSet = do
  conn <- cPS
  ret <- withTransaction conn $ \ c -> quickQuery c
    "SELECT question, got_correct, asked_time, asked_time_nanosec, \
    \answered_time, answered_time_nanosec, question_select_method FROM \
    \ask_log WHERE question_set = ?" [toSql qSet]
  -- why does hdbc give integer columns as strings?!
  -- convert nanoseconds to picoseconds for TOD
  return . flip map ret $ \ row -> (fromSql $ row!!0, (
      TOD (read . fromSql $ row!!2) ((read . fromSql $ row!!3) * 1000),
      TOD (read . fromSql $ row!!4) ((read . fromSql $ row!!5) * 1000),
      fromSql $ row!!1
      )
    )

timePDiff :: ClockTime -> ClockTime -> Integer
timePDiff (TOD xs xp) (TOD ys yp) = (xs - ys) * pSecInSec + xp - yp

doQSelect :: QSelect -> QNewSelect -> (String, [Qna]) ->
  IO (Either String Qna)
doQSelect QSRandom _qSelect (_qSet, qnas) =
  fmap Right . evalRandIO $ choice qnas
doQSelect (QSLastCorrectDeltaTimes i) qSelect (qSet, qnas) = do
  -- find q which was gotten correct last two times asked which
  --    most recently had timeSinceLast exceed i * (tSL - tSOneBeforeLast)
  -- if none match, pick randomly out of others or first in file
  -- (based on qSelect)
  --
  -- maybe later we will implement more fully:
  --   if none match, find q which was most recently gotten correct 1 but not 2
  --   if none match, find q which was gotten wrong most recently
  --   if none match, find q
  --   if none match, return Nothing
  --
  -- always use timeShowed for simplicity
  qSetHistory <- getQSetHistory qSet
  now <- getClockTime
  let
    -- make place for every possible question
    qMap = M.fromList $ mapAccum (\ (q, a) n -> (q, ([], n))) qnas
    -- populate history
    historyMap = M.fromListWith (++) $ map (second (:[])) qSetHistory
    -- only consider history items for currently existing questions
    qhMap = M.differenceWith (\ (_, n) h -> Just (h, n)) qMap historyMap
    -- sort histories most recent first
    qho = M.map
      (first $ sortBy (\(tS, tA, b) (tS2, tA2, b2) -> compare tS2 tS))
      qhMap
    -- anything answered too recently is off limits
    qNonRecent = M.filter (\ (v, n) -> and $
      map (\(tS, tA, b) -> timePDiff now tA > 2 * 60 * 60 * pSecInSec) v) qho
    -- find non-recent qnas where last two were correct
    lastTwoTrue (l, n) = case l of
      [] -> False
      [a] -> False
      (tS, tA, b):(tS2, tA2, b2):rest -> b && b2
    (tt, nTt) = M.partition lastTwoTrue qNonRecent
    tDiff ((tS, tA, b):(tS2, tA2, b2):rest, n) =
      fromIntegral (timePDiff now tS) - i * fromIntegral (timePDiff tS tS2)
    ttTDiff = M.filter (> 0) $ M.map tDiff tt
    -- helper fcn: return the Qna with question q
    rQsOf q = return . Right . fromJust $ lookupWithKey q qnas
  if M.null ttTDiff
    then if M.null nTt
      -- prevent illicit postlearning (is this good or bad)
      then clrScr >> return (Left
        "All questions correct or seen too recently!  Mem \
        \something else for a bit..")
      else case qSelect of
        QNSInitial -> rQsOf . fst . minimumBy (compare `on` (snd . snd)) $
          M.toList nTt
        QNSRandom -> evalRandIO . choice $ M.keys nTt >>= rQsOf
    -- TODO: use minimumBy!
    else rQsOf . fst . head . sortBy (compare `on` snd) $ M.toList ttTDiff

recordQ :: String -> QNewSelect -> QSelect -> String -> String -> 
  (ClockTime, ClockTime, Bool) -> IO ()
recordQ answerer qSelect askMethod qSet q (TOD tSs tSp, TOD tAs tAp, b) = do
  conn <- cPS
  ret <- withTransaction conn $ \ c -> run c 
    "INSERT INTO ask_log (question_set, question, got_correct, asked_time, \
    \asked_time_nanosec, answered_time, answered_time_nanosec, \
    \question_select_method, question_sub_select_method, answerer) VALUES \
    \(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    [
      toSql qSet, toSql q, toSql b, SqlInteger tSs, 
      SqlInteger $ tSp `div` 1000, SqlInteger tAs, 
      SqlInteger $ tAp `div` 1000, toSql $ dbStr askMethod, 
      toSql $ dbStr qSelect, toSql answerer
      ]
  disconnect conn

askQs :: String -> QSelect -> QNewSelect -> (String, [Qna]) -> String -> 
  InputT IO ()
askQs answerer askMethod qSelect qqs@(qSet, qnas) comm = do
  qOrErr <- io $ doQSelect askMethod qSelect qqs
  case qOrErr of
    Left e -> io $ putStrLnF e
    Right q -> do
      (tS, tA, b, thenQuit) <- askQ q comm
      io $ recordQ answerer qSelect askMethod qSet (fst q) (tS, tA, b)
      if thenQuit
        then
          -- prevent illicit postlearning (is this good or bad)
          io clrScr
        else askQs answerer askMethod qSelect qqs comm

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
  Option ['u'] ["user"] (ReqArg FUser "USER")  "user name",
  Option ['a'] ["askmethod"] (ReqArg FQSelect "r|d")
    "askmethod: random or doubling",
  Option ['q'] ["qselect"] (ReqArg FQNewSelect "r|i")
    "qselect: random or initial",
  Option ['h'] ["max"] (ReqArg FMaxLine "N")  "maximum line number"]
type OptVals = (String, QSelect, QNewSelect, Maybe Int)

procOpt :: Flag -> OptVals -> OptVals
procOpt s (a, m, q, n) = case s of
  FUser answerer -> (answerer, m, q, n)
  FQSelect meth -> case meth of
    "r" -> (a, QSRandom, q, n)
    "d" -> (a, QSLastCorrectDeltaTimes 2, q, n)
    _ -> error "not a valid askmethod"
  FQNewSelect qnas -> case qnas of
    "r" -> (a, m, QNSRandom, n)
    "i" -> (a, m, QNSInitial, n)
    _ -> error "not a valid qselect"
  FMaxLine n' -> (a, m, q, Just $ read n')

main :: IO ()
main = runInputT defaultSettings $ do
  args <- io getArgs
  let
    header = "Usage:"
    (opts, qnaFNs) = case getOpt Permute options args of
      (o, n, []) -> (o, n)
      (_, _, errs) -> error $ concat errs ++ usageInfo header options
    -- currently must specify exactly one mem file
    [qnaFN, comm] = if length qnaFNs == 1 then qnaFNs ++ ["false"] else qnaFNs
    (answerer, askMethod, qSelect, maxLineMby) =
      foldr procOpt ("", QSLastCorrectDeltaTimes 2, QNSRandom, Nothing) opts
  qnaF <- io $ openFile qnaFN ReadMode
  c <- io $ hGetContents qnaF
  let
    ls = case maxLineMby of
      Nothing -> lines c
      Just maxLine -> take maxLine $ lines c
  case readQs $ ls of
    Left e -> io . putStrLnF $ "error parsing " ++ qnaFN ++ ":" ++ e
    Right r -> askQs answerer askMethod qSelect r comm

