{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified Data.Text.Read

data Color = Red Int | Green Int | Blue Int deriving Show
type Round = [Color]
data Game = Game { gameId :: Int, rounds :: [Round] } deriving Show

solve lines = sum $ map minCubes $ map rounds games
    where games = map parseLine lines

parseLine :: T.Text -> Game
parseLine line = Game gameId $ map parseRound rounds
    where gameId = read . T.unpack . snd . T.breakOnEnd " " $ T.dropEnd 1 l1 :: Int
          rounds = map (T.drop 1) $ T.splitOn ";" l2
          (l1, l2) = T.breakOnEnd ":" line

parseRound :: T.Text -> Round
parseRound txt = map parseColor xs
    where xs = T.splitOn ", " txt

parseColor txt = case Data.Text.Read.decimal txt of
    Right(n, " blue") -> Blue n
    Right(n, " green") -> Green n
    Right(n, " red") -> Red n
    -- I just assume correct input here

minCubes :: [Round] -> Int
minCubes xs = minRed * minGreen * minBlue
    where minRed = maximum [ n | x@(Red n) <- concat xs ]
          minGreen = maximum [ n | x@(Green n) <- concat xs ]
          minBlue = maximum [ n | x@(Blue n) <- concat xs ]

main = do
    str <- IO.readFile "input.txt"
    print $ solve . T.lines $ str
