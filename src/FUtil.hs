module FUtil where

import Control.Applicative hiding ((<|>))
import Control.Arrow
import Control.Monad
import Control.Monad.Error
import Control.Monad.Random
import Data.Bits
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Time
import Data.Word
import Locale
import System.Cmd
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Process
import System.Random
import Text.ParserCombinators.Parsec hiding (satisfy, oneOf, noneOf)
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified HSH

--
-- random
--

gens :: (RandomGen t) => t -> [t]
gens g = let (g1, g2) = split g in g1:gens g2

shuffle :: (RandomGen g) => [b] -> Rand g [b]
shuffle l = do
  rndInts <- getRandoms
  return . map snd . sortBy (compare `on` fst) $ zip (rndInts :: [Int]) l

choice :: (RandomGen g) => [b] -> Rand g b
choice l = fmap head $ shuffle l

--
-- lists
--

interlines :: [[Char]] -> [Char]
interlines = intercalate "\n"
interwords :: [[Char]] -> [Char]
interwords = intercalate " "
intertabs :: [[Char]] -> [Char]
intertabs = intercalate "\t"

padl :: a -> Int -> [a] -> [a]
padl c l s = replicate (l - length s) c ++ s

padr :: a -> Int -> [a] -> [a]
padr c l s = s ++ replicate (l - length s) c

splitN :: Int -> [a] -> [[a]]
splitN _ [] = []
splitN n xs = l : splitN n r where (l, r) = splitAt n xs

-- stripe one list across n of them
stripe :: Int -> [a] -> [[a]]
stripe n = reverse . foldr (\ x (ys:yss) -> yss ++ [x:ys]) (replicate n [])

-- pythonic list slicing
slice :: Int -> Int -> [a] -> [a]
slice x y l = take (y - x) $ drop x l

-- cyclic successor on an enum
cycSucc :: (Eq a, Bounded a, Enum a) => a -> a
cycSucc x = if x == maxBound then minBound else succ x

-- cycle a list one element forward
cyc :: [a] -> [a]
cyc (x:xs) = xs ++ [x]
-- cycle a list one element back
cycB :: [a] -> [a]
cycB = reverse . cyc . reverse

sublistIx :: Eq a => [a] -> [a] -> Maybe Int
sublistIx subl l = findIndex id $ map (subl `isPrefixOf`) (tails l)

-- substitute a sublist (e.g. string replace)
subst :: Eq a => [a] -> [a] -> [a] -> [a]
subst _ _ [] = []
subst from to xs@(a:as) =
  if from `isPrefixOf` xs
    then to ++ subst from to (drop (length from) xs)
    else a : subst from to as

lookupWithKey :: Eq a => a -> [(a, b)] -> Maybe (a, b)
lookupWithKey k l = case lookup k l of
  Just v -> Just (k, v)
  Nothing -> Nothing

cap :: [a] -> [a] -> [a]
cap c l = c ++ l ++ c

-- note the return is slight different from break
-- (is it wrong that i have always wanted break to give Maybe ([a], [a])
-- and not include the broken-out part)
breakMb :: (a -> Bool) -> [a] -> Maybe ([a], [a])
breakMb f l = let (x, y) = break f l in
  if null y then Nothing else Just (x, tail y)

breaks :: (a -> Bool) -> [a] -> [[a]]
breaks f l = if null b then [a] else (a:breaks f (drop 1 b))
  where (a, b) = break f l

breaksN :: (a -> Bool) -> Int -> [a] -> [[a]]
breaksN _ 0 l = [l]
breaksN f n l = if null b then [a] else (a:breaksN f (n - 1) (drop 1 b))
  where (a, b) = break f l

breakSubl :: Int -> ([a] -> Bool) -> [a] -> Maybe ([a], [a])
breakSubl n f l = if f (take n l)
  then Just ([], drop n l)
  else case l of
    [] -> Nothing
    (x:xs) -> do
      (a, b) <- breakSubl n f xs
      return (x:a, b)

breakOnSubl :: (Eq a) => [a] -> [a] -> Maybe ([a], [a])
breakOnSubl subl = breakSubl (length subl) (== subl)

-- perform a "break-like" function repeatedly
breaklikes :: ([a] -> Maybe ([a], [a])) -> [a] -> [[a]]
breaklikes f xs = case f xs of
  Nothing -> [xs]
  Just (l, r) -> l:breaklikes f r

breaksOnSubl :: (Eq a) => [a] -> [a] -> [[a]]
breaksOnSubl subl = breaklikes (breakOnSubl subl)

max0 :: (Num a, Ord a) => [a] -> a
max0 l = if null l then 0 else maximum l

ltrim :: String -> String
ltrim = dropWhile (== ' ')

allBreak :: [a] -> [([a], [a])]
allBreak s = zip (inits s) (tails s)

uncalate :: Eq a => [a] -> [a] -> [[a]]
uncalate [] s = map (:[]) s  -- tricksy case to remember..
uncalate glue s = case find (isPrefixOf glue . snd) $ allBreak s of
  Nothing -> [s]
  Just (s1, s2) -> s1:(uncalate glue $ drop (length glue) s2)

groupByAdj :: (a -> a -> Bool) -> [a] -> [[a]]
groupByAdj _ [] = [[]]
groupByAdj f (x:xs) = groupByAdjPart f xs [x] where
  groupByAdjPart f [] part = [part]
  groupByAdjPart f xa@(x:xs) part = if f (last part) x
    then groupByAdjPart f xs $ part ++ [x]
    else part:groupByAdj f xa

-- like unix comm
comm :: (Ord a) => [a] -> [a] -> (([a], [a]), [a])
comm xa@(x:xs) ya@(y:ys) = case compare x y of
  EQ -> second ((x:)) $ comm xs ys
  LT -> first (first (x:)) $ comm xs ya
  GT -> first (second (y:)) $ comm xa ys

uncons :: [a] -> (a, [a])
uncons (x:xs) = (x, xs)
uncons [] = error "uncons: empty list"

headTails :: [[a]] -> ([a], [[a]])
headTails = unzip . map uncons

-- Reversify (work from end instead of beginning) a function.
reversify :: ([a] -> [a]) -> [a] -> [a]
reversify f = reverse . f . reverse

-- Reversify (work from end instead of beginning) a function that makes a tuple
-- (such as span).
reversifyTup :: ([a] -> ([b], [b])) -> [a] -> ([b], [b])
reversifyTup f = swap . bothond reverse . f . reverse

reversifyFTup :: (Functor f) => ([a] -> f ([b], [b])) -> [a] -> f ([b], [b])
reversifyFTup f = (swap . bothond reverse <$>) . f . reverse

onHead :: (a -> a) -> [a] -> [a]
onHead _ [] = []
onHead f (x:xs) = f x : xs

--
-- functions
--

frep :: (a -> a) -> Int -> a -> a
frep f n x = iterate f x !! n

bothond :: (Arrow a) => a b c -> a (b, b) (c, c)
bothond f = f *** f

--
-- IO, monads, time
--

io :: MonadIO m => IO a -> m a
io = liftIO

nothErr :: (MonadError e m) => e -> Maybe a -> m a
nothErr err = maybe (throwError err) return

inCd :: FilePath -> IO b -> IO b
inCd dir f = do
  dirOrig <- HSH.pwd
  HSH.cd dir
  res <- f
  HSH.cd dirOrig
  return res

shEsc :: String -> String
shEsc s = "'" ++ f s ++ "'" where
  f "" = ""
  f ('\'':s) = "'\\''" ++ f s
  f ('\\':s) = "\\\\" ++ f s
  f (x:s) = [x] ++ f s

doArgs :: String -> c -> [OptDescr (c -> c)] -> IO (c, [String])
doArgs header defOpts options = do
  args <- getArgs
  return $ case getOpt Permute options args of
    (o, n, []) -> (foldl (flip id) defOpts o, n)
    (_, _, errs) -> error $ concat errs ++ usageInfo header options


globsOrNot :: [String] -> IO [String]
globsOrNot = fmap concat . mapM (\ arg -> do
  gs <- HSH.glob arg
  return $ case gs of
    [] -> [arg]
    gs -> gs)

ifM :: (Monad m) => m Bool -> m b -> m b -> m b
ifM c t e = c >>= \ r -> if r then t else e

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM t = (t >>=) . flip when

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM t = (t >>=) . flip unless

logTimeStr :: IO String
logTimeStr = do
  t <- getCurrentTime
  return $ formatTime defaultTimeLocale "%Z %Y-%m-%d %H:%M:%S.%q" t

-- try to get user input until input-parsing fcn succeeds
repInp :: String -> (String -> Either String a) -> IO a
repInp dispStr parseFcn = do
  putStr dispStr
  hFlush stdout
  s <- getLine
  case parseFcn s of
    Left err -> do
      putStrLn err
      repInp dispStr parseFcn
    Right ret -> do
      return ret

clrScr :: IO ()
clrScr = do
  system "clear"
  return ()

noArgs :: IO ()
noArgs = do
  args <- getArgs
  unless (null args) $ do
    progName <- getProgName
    error $ "usage: " ++ progName ++ " takes no arguments"

mapAccum :: (a -> Int -> b) -> [a] -> [b]
mapAccum = mapAccumFromN 0 where
  mapAccumFromN :: Int -> (a -> Int -> b) -> [a] -> [b]
  mapAccumFromN n f l = case l of
    [] -> []
    (x:xs) -> [f x n] ++ (mapAccumFromN (n + 1) f xs)

pSecInSec :: Integer
pSecInSec = 1000 ^ 4

readFileStrict :: String -> IO String
readFileStrict f = do
  c <- readFile f
  length c `seq` return c

--
-- boilerplate
--

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "FUtil.fromRight: was Left.."

fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft _ = error "FUtil.fromLeft: was Right.."

--
-- backwards variants
--

dlof :: a -> [a -> a] -> a
dlof = foldl (flip ($))

pam :: a -> [a -> b] -> [b]
pam x = map ($ x)

--
-- interact variants
--

interactL :: ([String] -> [String]) -> IO ()
interactL f = interact (unlines . f . lines)

onRight :: (b -> c) -> Either a b -> Either a c
onRight f (Left a) = Left a
onRight f (Right b) = Right (f b)

interactOrErr :: Show err => (String -> Either err String) -> IO ()
interactOrErr f = do
  s <- getContents
  case f s of
    Left err -> hPutStr stderr (show err)
    Right out -> putStr out

interactLOrErr :: Show err => ([String] -> Either err [String]) -> IO ()
interactLOrErr f = interactOrErr (onRight unlines . f . lines)

--
-- parsing
--

type StreamParser tok st = GenParser tok st [tok]

satisfy :: Show tok => (tok -> Bool) -> GenParser tok st tok
satisfy f = tokenPrim show (\ pos _ _ -> pos) $ \ tok ->
  case f tok of
    True -> Just tok
    False -> Nothing

oneOf, noneOf :: (Eq tok, Show tok) => [tok] -> GenParser tok st tok
oneOf cs = satisfy (flip elem cs)
noneOf cs = satisfy (not . flip elem cs)

-- balance '(' ')' e.g. will parse "(()(()))" but not "())".
-- note it does not return outermost parens (l and r) (should it?).
balance, balanceEnd :: (Eq tok, Show tok) => tok -> tok -> StreamParser tok st
balance l r = satisfy (== l) >> balanceEnd l r
balanceEnd l r = do
  t <- anyToken
  if t == r then return [] else if t == l
    -- can we join?
    then liftM2 (\ ts ts' -> l:ts ++ r:ts') (balanceEnd l r) (balanceEnd l r)
    else (t:) <$> balanceEnd l r

-- apply parser "wherever it works", passing rest of stream through unchanged
whereItWorks :: Show tok => StreamParser tok st -> StreamParser tok st
whereItWorks p = (eof >> return []) <|>
  liftM2 (++) (try p) (whereItWorks p) <|>
  liftM2 (:) anyToken (whereItWorks p)

-- apply parser "wherever it works", discarding the rest of the stream
onlyWhereItWorks :: Show tok => GenParser tok st [a] -> GenParser tok st [a]
onlyWhereItWorks p = (eof >> return []) <|>
  liftM2 (++) (try p) (onlyWhereItWorks p) <|>
  (anyToken >> onlyWhereItWorks p)

instance Error ParseError

--
-- display helpers
--

spaceTable :: [[String]] -> [String]
spaceTable [] = []
spaceTable ([]:_) = []
spaceTable t@([_]:_) = map head t  -- one col
spaceTable t = zipWith (++) (spaceBlock col) $ spaceTable rest where
  (col, rest) = unzip $ map (\ (x:xs) -> (x, xs)) t

-- rename to spaceCol?
spaceBlock :: [String] -> [String]
spaceBlock b = let
    lens = map length b
    w = max0 lens in
  zipWith (++) b $ map (\ l -> take (w - l) $ repeat ' ') lens

-- if you want to equally-space several blocks but keep them separate
spaceBlocks :: [[String]] -> [[String]]
spaceBlocks bs = let
    lenss = map (map length) bs
    w = max0 $ map max0 lenss in
  zipWith
   (\ b lens -> zipWith (++) b $ map (\ l -> take (w - l) $ repeat ' ') lens)
    bs lenss

-- how is this not done for me by ghc
-- Convert Unicode characters to UTF-8.
toUtf :: String -> String
toUtf [] = []
toUtf (x:xs)
  | ord x <= 0x007F = x:toUtf xs
  | ord x <= 0x07FF =
    chr (0xC0 .|. ((ord x `shift` (-6)) .&. 0x1F)):
    chr (0x80 .|. (ord x .&. 0x3F)):
    toUtf xs
  | otherwise =
    chr (0xE0 .|. ((ord x `shift` (-12)) .&. 0x0F)):
    chr (0x80 .|. ((ord x `shift` (-6)) .&.  0x3F)):
    chr (0x80 .|. (ord x .&. 0x3F)):
    toUtf xs

fromUtf :: String -> String
fromUtf [] = []
fromUtf (all@(x:xs)) | ord x<=0x7F = x:fromUtf xs
                     | ord x<=0xBF = err
                     | ord x<=0xDF = twoBytes all
                     | ord x<=0xEF = threeBytes all
                     | otherwise   = err
  where
    twoBytes (x1:x2:xs) = chr (((ord x1 .&. 0x1F) `shift` 6) .|.
                               (ord x2 .&. 0x3F)):fromUtf xs
    twoBytes _ = error "fromUTF: illegal two byte sequence"

    threeBytes (x1:x2:x3:xs) = chr (((ord x1 .&. 0x0F) `shift` 12) .|.
                                    ((ord x2 .&. 0x3F) `shift` 6) .|.
                                    (ord x3 .&. 0x3F)):fromUtf xs
    threeBytes _ = error "fromUTF: illegal three byte sequence"

    err = error "fromUTF: illegal UTF-8 character"

--
-- byte shuffling
--

packBytes :: [Word8] -> Int
packBytes = sum . zipWith (*) (iterate (256 *) 1) . map fromIntegral

--
-- power set kind of things
--

powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x:xs) = l ++ map (x:) l where l = powerList xs

type MetaSet a = S.Set (S.Set a)

-- there isn't really anything cooler/set-specific for here, oddly
powerSet :: Ord a => S.Set a -> MetaSet a
powerSet = S.fromList . map S.fromList . powerList . S.toList

allPoss :: (Bounded a, Enum a) => [a]
allPoss = enumFrom minBound

allListsOf :: [a] -> [[a]]
allListsOf [] = [[]]
allListsOf xs = as where as = []:[a ++ [x] | a <- as, x <- xs]

dirProd :: [a] -> [b] -> [(a, b)]
dirProd xs ys = [(x, y) | x <- xs, y <- ys]

--
-- maybe
--

readMb :: Read a => String -> Maybe a
readMb s = fmap fst . listToMaybe $ reads s

--
-- errors
--

type CanErrStr a = Either String a

--
-- subprocesses
--

cmdOutput :: String -> [String] -> IO (CanErrStr BS.ByteString)
cmdOutput cmd args = do
  (inp,out,err,pid) <- runInteractiveProcess cmd args Nothing Nothing
  o <- BS.hGetContents out
  ret <- waitForProcess pid
  case ret of
    ExitFailure e -> return . Left $ show e
    ExitSuccess -> return $ Right o

--
-- tuples
--

swap :: (t, t1) -> (t1, t)
swap (x, y) = (y, x)

seqTupL :: ((t, t1), t2) -> ((t, t2), (t1, t2))
seqTupL ((x, y), z) = ((x, z), (y, z))

seqTupR :: (t, (t1, t2)) -> ((t, t1), (t, t2))
seqTupR (x, (y, z)) = ((x, y), (x, z))

--
-- Map
--

flipMap :: (Ord k, Ord v) => M.Map k v -> M.Map v (S.Set k)
flipMap = M.fromListWith S.union . map (second S.singleton . swap) . M.toList
