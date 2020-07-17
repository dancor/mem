{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Codec.Serialise (Serialise, readFileDeserialise, writeFileSerialise)
import Codec.Serialise.IO (readFileDeserialise, writeFileSerialise)
import Control.Arrow (first, second)
import Control.Monad (liftM2, unless)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Maybe (isJust, fromJust)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Process (system)
import System.Random (randomRIO)

type MyTime = Double
type Q = Text
type A = Text
data QSched = QSched
  { qSched   :: MyTime
  , qLastSaw :: MyTime
  , qLastSawWasCorrect :: Bool
  } deriving Generic
instance Serialise QSched
type Sched = HashMap Q QSched

getMyTime :: IO MyTime
getMyTime = realToFrac <$> getPOSIXTime

showSecs s | s < 0 = '-' : showSecs (negate s)
           | s < 60 = show (round s) ++ "s"
showSecs s | s < 3600 = show (round $ s / 60) ++ "m"
showSecs s | s < 24 * 3600 = show (round $ s / 3600) ++ "h"
showSecs s | otherwise = show (round $ s / 3600 / 24) ++ "d"

mainOnArgs args = do
    case args of
      [schedF] -> do
        a <- readFileDeserialise schedF
        t <- getMyTime
        print t
        let info = map (\(q, sched) ->
                (qSched sched - t, q)) (HM.toList (a :: Sched))
        mapM_ (\(s, q) -> T.putStrLn $ T.pack (showSecs s) <> "\t" <> q) $
            sortBy (comparing fst) info
      _ -> error "Usage: dump <schedule-file>"

main = getArgs >>= mainOnArgs 
