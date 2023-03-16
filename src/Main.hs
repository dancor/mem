{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Codec.Serialise (Serialise, readFileDeserialise, writeFileSerialise)
import Codec.Serialise.IO (readFileDeserialise, writeFileSerialise)
import Control.Arrow (first, second)
import Control.Concurrent (threadDelay)
import Control.Monad (liftM2, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (minimumBy, partition)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import System.Console.Haskeline (defaultSettings, getInputLine, InputT,
    runInputT)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO
import System.Process (system)
import System.Random (randomRIO)

-- Probably will manipulate time as Double and save it that way. Old notes:
-- Store unix milliseconds as Int64? Note that storing deciseconds as Int32
-- overflowed in 1976, so we're not doing that. We could store day numbers but
-- then timezone changes are tricky. Float usually shifts the current time up
-- to about 64 seconds in 2020, which is probably just about precise enough for
-- us but still concerning (2min in 2071, 4min in 2172, 9min in 2374).

type MyTime = Double
type Q = Text
type A = Text
type Qna = (Q, A)
type Qnas = [Qna]
data QSched = QSched
  { qSched   :: MyTime
  , qLastSaw :: MyTime
  , qLastSawWasCorrect :: Bool
  } deriving Generic
instance Serialise QSched
type Sched = HashMap Q QSched

io :: IO a -> InputT IO a
io = liftIO

getMyTime :: IO MyTime
getMyTime = realToFrac <$> getPOSIXTime

parseQna :: Text -> Text -> Qna
parseQna setName l = case T.break (== '|') l of
  (_, "") -> error $ "Could not parse question-and-answer line: " ++ show l
  (q, sepA) -> (setName <> "\0" <> q, T.tail sepA)

parseQnaFile :: Text -> Qnas
parseQnaFile c = map (parseQna setName) . filter (not . ("#" `T.isPrefixOf`)) $
    filter (not . T.null) ls
  where
    (l:ls) = T.lines c
    setPre = "# question set: "
    setName = if setPre `T.isPrefixOf` l then T.drop (T.length setPre) l
      else error "first line is not of form: # question set: "

partitionFstMaybe :: [(Maybe a, b)] -> ([(a, b)], [b])
partitionFstMaybe = first (map $ first fromJust) . second (map snd) .
    partition (isJust . fst)

seenUnseen :: Sched -> [Qnas] -> ([(QSched, Qna)], [Qnas])
seenUnseen sched = first concat . unzip .
    map (partitionFstMaybe . map (\qna@(q,_) -> (q `HM.lookup` sched, qna)))

whileM :: IO Bool -> IO () -> IO ()
whileM t a = t >>= \r -> if r then a >> whileM t a else return ()

waitWhileKeyDown :: IO ()
waitWhileKeyDown = do
    threadDelay 200000
    r <- hReady stdin
    when r $ whileM (hReady stdin) (getChar >> return ()) >> waitWhileKeyDown

myGetInputLine :: InputT IO (Maybe String)
myGetInputLine = io waitWhileKeyDown >> getInputLine ""

asks :: FilePath -> Sched -> [Qnas] -> InputT IO ()
asks schedF sched qnas = do
    io $ system "clear"
    t <- io getMyTime
    let (seen, unseenByFile) = seenUnseen sched qnas
        (ready, notReady) = partition ((< t) . qSched . fst) seen
        (notReadyLastCorrect, notReadyLastWrong) = partition (qLastSawWasCorrect . fst) notReady
        notReadyLastCorrectDueHour = filter ((< t + 3600) . qSched . fst) notReadyLastCorrect
        notReadyLastCorrectDue2Hour = filter ((< t + 2 * 3600) . qSched . fst) notReadyLastCorrect
        notReadyLastCorrectDue24Hour = filter ((< t + 24 * 3600) . qSched . fst) notReadyLastCorrect
        askOldest = ask . first Just . minimumBy (comparing $ qSched . fst)
        --randEl l = (l !!) <$> randomRIO (0, length l - 1)
        randEl = return . head
        ask (schedMb, qna@(q, a)) = do
            io . T.putStrLn $ T.dropWhile (/= '\0') q <> "\t\t" <> 
                T.intercalate ":" (map (T.pack . show) [length ready, 
                sum $ map length unseenByFile, length notReadyLastWrong,
                length notReadyLastCorrectDueHour, 
                length notReadyLastCorrectDue2Hour,
                length notReadyLastCorrectDue24Hour])
            aTry <- T.pack . fromMaybe "" <$> myGetInputLine
            io $ T.putStrLn ""
            io . T.putStrLn $ T.replace "<br>" "\n\n" $ T.replace "   " "\n\n" a
            io . T.putStrLn $ if aTry == a then "Correct!" else "DIDN'T MATCH."
            r <- fromMaybe ("" :: String) <$> myGetInputLine
            let (correct, quit) = case r of
                  ""  -> (True , False)
                  "q" -> (True , True )
                  "Q" -> (False, True )
                  _   -> (False, False)
            t2 <- io $ getMyTime
            let nextTime = case (correct, schedMb) of
                  (False, _) -> t2 + 5 * 60
                  (_, Just (QSched _ lastSaw True)) -> t2 + 2 * (t2 - lastSaw)
                  _ -> t2 + 8 * 3600
                sched2 = HM.insert q (QSched nextTime t2 correct) sched
            io $ writeFileSerialise schedF sched2
            unless quit $ asks schedF sched2 qnas
    case (ready, filter (not . null) unseenByFile, notReadyLastWrong) of 
      ([], [] , []) -> io $ T.putStrLn "Done for now!"
      ([], [] , _ ) -> askOldest notReadyLastWrong
      ([], u:_, _ ) -> io (randEl u) >>= ask . (,) Nothing
      _             -> askOldest ready

mainOnArgs :: [String] -> IO ()
mainOnArgs args = case args of
  schedF:qnaFs -> do
    fExists <- doesFileExist schedF
    unless fExists $ writeFileSerialise schedF (HM.empty :: Sched)
    sched <- readFileDeserialise schedF
    qnaLists <- map parseQnaFile <$> mapM T.readFile qnaFs
    runInputT defaultSettings $ asks schedF sched qnaLists
  _ -> error $ "Usage: mem <schedule-file> <question-and-answer-files>\n" ++ 
    "Args were: " ++ show args

main :: IO ()
main = getArgs >>= mainOnArgs 
