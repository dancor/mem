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
import Control.Arrow
import Control.Monad.Error
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Database.HDBC
import Database.HDBC.PostgreSQL
import System
import System.Console.GetOpt
import System.Environment
import System.IO
import qualified System.Process as SP
import System.Random
import System.Time
import Util

type Qna = (String, String)
-- this is the over-arching selection method (concerning history)
data AskMethod = Random | LastCorrectDeltaTimes Float
instance Show AskMethod where
  show Random = "random"
  show (LastCorrectDeltaTimes i) = "lastCorrectDeltaTimes " ++ show i
-- this is the secondary selection method (when history doesn't say which)
data QSelect = QSRandom | QSInitial
instance Show QSelect where
  show QSRandom = "random"
  show QSInitial = "initial"

putStrLnF s = putStrLn s >> hFlush stdout

askQ :: Qna -> String -> IO (ClockTime, ClockTime, Bool, Bool)
askQ (q, a) comm = do
  clrScr
  putStrLnF q
  tS <- getClockTime
  r <- getLine
  tA <- getClockTime
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
  putStrLnF $ "\n" ++ a
  (pI, pO, pE, pH) <- SP.runInteractiveCommand (comm ++ " " ++ q)
  pC <- hGetContents pO
  putStr pC
  --SP.runCommand (comm ++ " " ++ q)
  --putStrLnF (comm ++ " " ++ q)
  putStrLnF "\nCorrect (enter for yes, q for yes and quit, else for no)?"
  c <- getLine
  return $ (tS, tA, c == "" || c == "q", c == "q")

cPS = handleSqlError $ connectPostgreSQL "dbname=memorization"

getQSetHistory :: String -> IO [(String, (ClockTime, ClockTime, Bool))]
getQSetHistory qSet = do
  conn <- cPS
  ret <- withTransaction conn (\conn -> do
    quickQuery conn "SELECT question, got_correct, asked_time, asked_time_nanosec, answered_time, answered_time_nanosec, question_select_method FROM ask_log WHERE question_set = ?" [toSql qSet]
    )
  -- why the fuck does hdbc give me integer columns as fucking strings
  -- convert nanoseconds to picoseconds for TOD
  return $ map (\row -> (fromSql $ row!!0, (TOD (read $ fromSql $ row!!2) ((read $ fromSql $ row!!3) * 1000), TOD (read $ fromSql $ row!!4) ((read $ fromSql $ row!!5) * 1000), fromSql $ row!!1))) ret

timePDiff :: ClockTime -> ClockTime -> Integer
timePDiff (TOD xs xp) (TOD ys yp) = (xs - ys) * pSecInSec + xp - yp

doAskMethod :: AskMethod -> QSelect -> (String, [Qna]) -> IO (Either String Qna)
doAskMethod askMethod qSelect (qSet, qs) = case askMethod of
  Random -> do
    c <- chooseR qs
    return $ Right c
  LastCorrectDeltaTimes i -> do
    -- find q which was gotten correct last two times asked which
    --    most recently had timeSinceLast exceed i * (tSL - tSOneBeforeLast)
    -- if none match, pick randomly out of others or first in file (based or qSelect)
    -- maybe later?
    --   if none match, find q which was most recently gotten correct 1 but not 2
    --   if none match, find q which was gotten wrong most recently
    --   if none match, find q
    --   if none match, return Nothing
    -- always use timeShowed for simplicity
    r <- getQSetHistory qSet
    now <- getClockTime
    let
      -- make place for every possible question
      qMap = M.fromList $ mapAccum (\(q, a) n -> (q, ([], n))) qs
      -- populate history
      hMap = M.fromListWith (++) $ map (\(x, y) -> (x, [y])) r
      -- only consider history items corresponding to currently existing questions
      qhMap = M.differenceWith (\(_, n) h -> Just (h, n)) qMap hMap
      -- sort histories most recent first
      qho = M.map (first (sortBy (\(tS, tA, b) (tS2, tA2, b2) -> compare tS2 tS))) qhMap
      -- anything answered in last 10 hours is off limits
      qNonRecent = M.filter (\(v, n) -> and $ map (\(tS, tA, b) -> timePDiff now tA > 10 * 60 * 60 * pSecInSec) v) qho
      -- find non-recent qs where last two were correct
      lastTwoTrue (l, n) = case l of
        [] -> False
        [a] -> False
        (tS, tA, b):(tS2, tA2, b2):rest -> b && b2
      (tt, nTt) = M.partition lastTwoTrue qNonRecent
      tDiff ((tS, tA, b):(tS2, tA2, b2):rest, n) = fromIntegral (timePDiff now tS) - i * fromIntegral(timePDiff tS tS2)
      ttTDiff = M.filter (> 0) $ M.map tDiff tt
      -- helper fcn
      rQsOf k = return $ Right $ fromJust $ lookupWithKey k qs
    if M.null ttTDiff
      then
        if M.null nTt
          then do
            -- prevent illicit postlearning (is this good or bad)
            clrScr
            return $ Left "All questions correct or seen too recently!  Mem something else for a bit.."
          else case qSelect of
            QSInitial -> rQsOf $ fst $ minimumBy (compare `on` (snd . snd)) $ M.toList nTt
            QSRandom -> chooseR $ M.keys nTt >>= rQsOf
      -- TODO: use minimumBy!
      else rQsOf $ fst $ head $ sortBy (compare `on` snd) $ M.toList ttTDiff

recordQ :: String -> QSelect -> AskMethod -> String -> String -> (ClockTime, ClockTime, Bool) -> IO ()
recordQ answerer qSelect askMethod qSet q (TOD tSs tSp, TOD tAs tAp, b) = do
  conn <- cPS
  ret <- withTransaction conn (\conn -> do
    run conn "INSERT INTO ask_log (question_set, question, got_correct, asked_time, asked_time_nanosec, answered_time, answered_time_nanosec, question_select_method, question_sub_select_method, answerer) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      [toSql qSet, toSql q, toSql b, SqlInteger tSs, SqlInteger $ tSp `div` 1000, SqlInteger tAs, SqlInteger $ tAp `div` 1000, toSql $ show askMethod, toSql $ show qSelect, toSql answerer])
  --putStrLnF $ show ret
  disconnect conn

askQs :: String -> AskMethod -> QSelect -> (String, [Qna]) -> String -> IO ()
askQs answerer askMethod qSelect qqs@(qSet, qs) comm = do
  qOrErr <- doAskMethod askMethod qSelect qqs
  case qOrErr of
    Left e -> do
      putStrLnF e
      return ()
    Right q -> do
      (tS, tA, b, thenQuit) <- askQ q comm
      recordQ answerer qSelect askMethod qSet (fst q) (tS, tA, b)
      if thenQuit
        then
          -- prevent illicit postlearning (is this good or bad)
          clrScr
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

readQs :: [String] -> Either String (String, [Qna])
readQs s = let qSetPrefix = "# question set: " in
  if length s >= 1 && isPrefixOf qSetPrefix (head s)
    then
      let ([prefAndQSet], rest) = splitAt 1 s
          qSet = drop (length qSetPrefix) prefAndQSet
          qsME = map readQ $ rest
          qsMOrErr = sequence $ qsME in
        case qsMOrErr of
          Left e -> Left $ show (fromJust (findIndex isLeft qsME) + 1) ++ ": " ++ e
          Right qsM -> Right (qSet, catMaybes qsM)
    else Left "1: no question set specification found"

data Flag = FUser String | FAskMethod String | FQSelect String | FMaxLine String
options :: [OptDescr Flag]
options = [
  Option ['u'] ["user"] (ReqArg FUser "USER")  "user name",
  Option ['a'] ["askmethod"] (ReqArg FAskMethod "r|d")
    "askmethod: random or doubling",
  Option ['q'] ["qselect"] (ReqArg FQSelect "r|i")
    "qselect: random or initial",
  Option ['h'] ["max"] (ReqArg FMaxLine "N")  "maximum line number"]
type OptVals = (String, AskMethod, QSelect, Maybe Int)

procOpt :: Flag -> OptVals -> OptVals
procOpt s (a, m, q, n) = case s of
  FUser answerer -> (answerer, m, q, n)
  FAskMethod meth -> case meth of
    "r" -> (a, Random, q, n)
    "d" -> (a, LastCorrectDeltaTimes 2, q, n)
    _ -> error "not a valid askmethod"
  FQSelect qs -> case qs of
    "r" -> (a, m, QSRandom, n)
    "i" -> (a, m, QSInitial, n)
    _ -> error "not a valid qselect"
  FMaxLine n' -> (a, m, q, Just $ read n')

main :: IO ()
main = do
  args <- getArgs
  (opts, qnaFNs) <- case getOpt Permute options args of
    (o, n, []) -> return (o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage:"
  -- currently must specify exactly one mem file
  let
    [qnaFN, comm] = if length qnaFNs == 1 then qnaFNs ++ [""] else qnaFNs
  let (answerer, askMethod, qSelect, maxLineMby) = foldr procOpt ("", LastCorrectDeltaTimes 2, QSInitial, Nothing) opts
  qnaF <- openFile qnaFN ReadMode
  c <- hGetContents qnaF
  let
    ls = case maxLineMby of
      Nothing -> lines c
      Just maxLine -> take maxLine $ lines c
  case readQs $ ls of
    Left e -> putStrLnF $ "error parsing " ++ qnaFN ++ ":" ++ e
    Right r -> askQs answerer askMethod qSelect r comm
