
data Race = Race { time :: Int, distance :: Int } deriving Show

solve :: [Race] -> Int
solve xs = foldl1 (*) $ map f xs
    where f (Race t d) = length . filter (>d) $ map (\x -> x * (t-x) ) [0..t]


parseRaces :: String -> [Race]
parseRaces s = [Race { time = read t, distance = read d } | (t, d) <- zip times distances]
    where [times, distances] = map (drop 1 . words) $ lines s

main = do
    str <- getContents
    let races = parseRaces str
    print $ solve races
