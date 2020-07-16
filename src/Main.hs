{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Codec.Serialise (Serialise, readFileDeserialise, writeFileSerialise)
import Codec.Serialise.IO (readFileDeserialise, writeFileSerialise)
import Control.Arrow (first, second)
import Control.Monad (liftM2, unless)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (maximumBy, partition)
import Data.Maybe (isJust, fromJust)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import System.Environment (getArgs)
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
data QSched = QSched
  { qSched   :: MyTime
  , qLastSaw :: MyTime
  , qLastSawWasCorrect :: Bool
  } deriving Generic
instance Serialise QSched

getMyTime :: IO MyTime
getMyTime = realToFrac <$> getPOSIXTime

procQna :: Text -> (Text, Text)
procQna l = case T.splitOn "|" l of
  [q, a] -> (q, a)
  _ -> error $ "Could not process question-and-answer line: " ++ show l

asks :: FilePath -> HashMap Q QSched -> HashMap Q A -> IO ()
asks schedF sched qnas = do
    system "clear"
    t <- getMyTime
    let (seen, unseen) = first (map $ first fromJust) . second (map snd) .
            partition (isJust . fst) .
            map (\qna@(q, _) -> (q `HM.lookup` sched, qna)) $ HM.toList qnas
        (ready, notReady) = partition ((> t) . qSched . fst) seen
        askLatest = ask . first Just . maximumBy (comparing $ qSched . fst)
        randEl l = (l !!) <$> randomRIO (0, length l - 1)
        ask (schedMb, qna@(q, a)) = do
            T.putStrLn q
            _ <- getLine
            T.putStrLn a
            r <- getLine
            let (correct, quit) = case r of
                  ""  -> (True , False)
                  "q" -> (True , True )
                  "Q" -> (False, True )
                  _   -> (False, False)
            t2 <- getMyTime
            let nextTime = case (correct, schedMb) of
                  (False, _) -> t2 + 5 * 60
                  (_, Just (QSched _ lastSaw True)) -> t2 + 2 * (t2 - lastSaw)
                  _ -> t2 + 8 * 3600
                sched2 = HM.insert q (QSched nextTime t2 correct) sched
            writeFileSerialise schedF sched2
            unless quit $ asks schedF sched2 qnas
    case (ready, unseen, notReady) of 
      ([], [], []) -> T.putStrLn "Done for now!"
      ([], [], _ ) -> askLatest notReady
      ([], _ , _ ) -> randEl unseen >>= ask . (,) Nothing
      _            -> askLatest ready

mainOnArgs args = do
    case args of
      [schedF, qnaF] -> do
        fExists <- doesFileExist schedF
        unless fExists $ writeFileSerialise schedF []
        liftM2 (asks schedF) (readFileDeserialise schedF) $
            HM.fromList . map procQna . filter (not . ("#" `T.isPrefixOf`)) .
            T.lines <$> T.readFile qnaF
      _ -> error "Usage: mem <schedule-file> <question-and-answer-file>"

main = getArgs >>= mainOnArgs 
