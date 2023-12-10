import Data.Char

solve xs = sum $ map parseLine xs

parseLine line = read [head digits, last digits]
    where digits = filter isDigit line

main = do
    str <- readFile "input.txt"
    print $ solve . lines $ str
