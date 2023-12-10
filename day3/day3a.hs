{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified Data.Text.Read

--size = 10 -- test input
size = 140

neighbourIdxDelta = [1,size+1,size,size-1,-1,-size-1,-size,-size+1]

solve txt = sum $ [x | (x, _, _) <- xs]
    where xs = [(x, y, z) | (x, y, z) <- numbers txt 0, isAdjacent txt y z]

isAdjacent :: T.Text -> Int -> Int -> Bool
isAdjacent txt idx len = any isSym xs
    where xs = concat . map (neighbours txt) $ [idx..idx+len-1]

neighbours :: T.Text -> Int -> [Char]
neighbours txt i = map (T.index txt) indices
    where indices = [x | x <- map (+i) neighbourIdxDelta, x >= 0, x < size*size]

numbers :: T.Text -> Int -> [(Int, Int, Int)]
numbers "" _ = []
numbers s pos = case Data.Text.Read.decimal s of
    Right(num, rest) -> (num, pos, len) : numbers rest (pos + len)
        where len = T.length s - T.length rest
    Left(_) -> numbers (T.drop 1 s) $ pos + 1

isSym :: Char -> Bool
isSym ch
    | ch == '.'  = False
    | isDigit ch = False
    | otherwise  = True

main = do
    str <- IO.readFile "input.txt"
    print $ solve . T.concat . T.lines $ str
