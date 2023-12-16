import Data.Char (isDigit)

solve :: Int -> Int -> Int
solve time distance = length . filter (>distance) $ xs
    where xs = map (\x -> x * (time-x) ) [0..time]

parse :: String -> (Int, Int)
parse s = (time, distance)
    where [time, distance] = map (read . filter isDigit) $ lines s

main = do
    str <- getContents
    let (time, distance) = parse str
    print $ solve time distance
