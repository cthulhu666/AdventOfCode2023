{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Text.Read (decimal)
import Data.List.NonEmpty (NonEmpty, fromList, toList)

data Almanac = Almanac { seeds :: NonEmpty Int, mappings :: NonEmpty Mapping } deriving Show
data MappingEntry = MappingEntry { dstRangeStart :: Int, srcRangeStart :: Int, rangeLen :: Int } deriving Show
type Mapping = NonEmpty MappingEntry

solve :: Almanac -> Int
solve a = minimum $ fmap f $ seeds a
    where f = mapSeedToLoc $ mappings a

parseAlmanac :: T.Text -> Almanac
parseAlmanac txt = Almanac { seeds, mappings }
    where
        xs = T.splitOn "\n\n" txt
        (h:t) = xs
        seeds = fromList $ parseSeeds $ h
        mappings = fromList $ map parseMapping $ t


parseSeeds :: T.Text -> [Int]
parseSeeds txt = [x | Right(x, _) <- xs]
    where
        (_, s) = T.breakOnEnd  ": " txt
        xs = fmap decimal $ T.words s

parseMapping :: T.Text -> Mapping
parseMapping txt = fromList xs
    where xs = fmap parseMappingEntry $ drop 1 $ T.lines txt


parseMappingEntry :: T.Text -> MappingEntry
parseMappingEntry txt = MappingEntry { dstRangeStart = x1, srcRangeStart = x2, rangeLen = x3 }
    where
        xs = fmap decimal $ T.words txt
        [x1,x2,x3] = [x | Right(x, _) <- xs]

fetchFromMappingEntry :: MappingEntry -> Int -> Maybe Int
fetchFromMappingEntry e i
    | i >= n && i < m   = Just (dstRangeStart e + i - n)
    | otherwise         = Nothing
    where
        n = srcRangeStart e
        m = n + rangeLen e

fetchFromMapping :: Mapping -> Int -> Int
fetchFromMapping m i = f (toList m) i
    where
        f [] i = i
        f (x:xs) i = case fetchFromMappingEntry x i of
            Nothing -> f xs i
            Just n -> n

mapSeedToLoc :: NonEmpty Mapping -> Int -> Int
mapSeedToLoc xs i = foldl (\a m -> fetchFromMapping m a) i xs

main = do
    str <- getContents
    let almanac = parseAlmanac . T.pack $ str
    print $ solve almanac
