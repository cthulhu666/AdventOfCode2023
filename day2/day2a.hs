{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified Data.Text.Read

data Color = Red Int | Green Int | Blue Int deriving Show
type Round = [Color]
data Game = Game { gameId :: Int, rounds :: [Round] } deriving Show

-- only 12 red cubes, 13 green cubes, and 14 blue cubes
maxRed = 12
maxGreen = 13
maxBlue = 14

solve lines = sum $ map gameId validGames
    where validGames = [g | g <- games, validGame g]
          games = map parseLine lines

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

validGame :: Game -> Bool
validGame (Game _ rounds) = all validRound rounds

validRound :: Round -> Bool
validRound = all validColor

validColor :: Color -> Bool
validColor (Red n) = n <= maxRed
validColor (Green n) = n <= maxGreen
validColor (Blue b) = b <= maxBlue

main = do
    str <- IO.readFile "input.txt"
    print $ solve . T.lines $ str
