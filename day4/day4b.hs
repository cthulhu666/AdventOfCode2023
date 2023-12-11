{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified Data.Text.Read
import qualified Data.Array as A

type Card = ([Int], [Int])
type Deck = A.Array Int Card
type Score = A.Array Int Int

solve :: Deck -> Int
solve deck = sum $ A.elems $ foldl (\s n -> play deck s n) scr [i..j]
    where scr = A.array b [(x, 1) | x <- [i..j]]
          b = A.bounds deck
          (i,j) = b

play :: Deck -> Score -> Int -> Score
play deck scr n = A.accum (\x _ -> x + q) scr [(i, ()) | i <- xs]
    where s  = score (deck A.! n)
          q  = scr A.! n
          xs = [(n + 1)..(n + s)]

score :: Card -> Int
score (xs, ys) = length [n | n <- xs, n `elem` ys]

parseDeck :: [T.Text] -> Deck
parseDeck xs = A.array (1, n) [(i, parseCard x) | (x, i) <- zip xs [1..n]]
    where n = length xs

parseCard :: T.Text -> Card
parseCard s = ([x | Right(x, _) <- xs], [y | Right(y, _) <- ys])
    where [s1, s2] = T.splitOn ":" s
          [s3, s4] = T.splitOn "|" s2
          xs = map Data.Text.Read.decimal $ T.words s3
          ys = map Data.Text.Read.decimal $ T.words s4

main = do
    str <- IO.readFile "input.txt"
    print $ solve . parseDeck $ T.lines str
