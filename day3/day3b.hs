{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified Data.Text.Read

-- (actual number, starting index, length)
type PartNumber = (Int, Int, Int)

--size = 10 -- test input
size = 140

neighbourIdxDelta = [1,size+1,size,size-1,-1,-size-1,-size,-size+1]

solve :: T.Text -> Int
solve txt = sum . map (gearRatio xs) $ gears txt
    where xs = numbers txt 0

gearRatio :: [PartNumber] -> Int -> Int
gearRatio ns i
    | length xs < 2 = 0
    | otherwise = foldl1 (*) xs
    where xs = [x | (x,y,z) <- ns, adjacent i (y,z)]

gears :: T.Text -> [Int]
gears txt = map (T.length . fst) $ T.breakOnAll "*" txt


neighbours :: Int -> [Int]
neighbours i = [x | x <- map (+i) neighbourIdxDelta, x >= 0, x < size*size]


numbers :: T.Text -> Int -> [PartNumber]
numbers "" _ = []
numbers s pos = case Data.Text.Read.decimal s of
    Right(num, rest) -> (num, pos, len) : numbers rest (pos + len)
        where len = T.length s - T.length rest
    Left(_) -> numbers (T.drop 1 s) $ pos + 1


adjacent :: Int -> (Int,Int) -> Bool
adjacent pos (i,j) = length ys > 0
    where
          ys = filter (\x -> x `elem` r) xs
          xs = neighbours pos
          r  = [i..i+j-1]

main = do
    str <- IO.readFile "input.txt"
    print $ solve . T.concat . T.lines $ str
