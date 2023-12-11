{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified Data.Text.Read

type Card = ([Int], [Int])

solve :: [Card] -> Int
solve = sum . map score

score :: Card -> Int
score (xs, ys) = round $ 2 ** (fromIntegral (length [n | n <- xs, n `elem` ys]) - 1)

parse :: T.Text -> Card
parse s = ([x | Right(x, _) <- xs], [y | Right(y, _) <- ys])
    where [s1, s2] = T.splitOn ":" s
          [s3, s4] = T.splitOn "|" s2
          xs = map Data.Text.Read.decimal $ T.splitOn  " " s3
          ys = map Data.Text.Read.decimal $ T.splitOn  " " s4

main = do
    str <- IO.readFile "input.txt"
    print $ solve $ map parse $ T.lines str
