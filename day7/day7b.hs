import Data.List (elemIndex, group, sort, sortOn, uncons)

data Card = CardA | CardK | CardQ | CardT | Card9 | Card8 | Card7 | Card6 | Card5 | Card4 | Card3 | Card2 | CardJ
    deriving (Bounded,Enum,Eq,Ord,Show)

ranks :: [Char] -> [Card]
ranks xs =
  case f xs of
    Just ys -> map toEnum ys
    Nothing -> error "Bam"
  where
    f = sequence . map (\x -> elemIndex x cards)
    cards = ['A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J']

data HandType = Five | Four  | Full  | Three  | TwoPair  | Pair  | HighCard
    deriving (Eq, Ord, Show)

type Hand = [Card]

bestHand :: Hand -> Hand
bestHand h =
    case uncons . sortOn handType $ possibleHands h of
        Just (x, _) -> x
        Nothing -> error "Bam"

possibleHands :: Hand -> [Hand]
possibleHands [] = [[]]
possibleHands (CardJ:xs) = [y:ys | y <- options, ys <- possibleHands xs]
    where options = filter (/= CardJ) [minBound..maxBound] :: [Card]
possibleHands (x:xs) = [x:ys | ys <- possibleHands xs]

handType :: Hand -> HandType
handType xs = case ys of
    [[x,_,_,_,_]] -> Five
    [[x,_,_,_],_] -> Four
    [[x,_,_],[y,_]] -> Full
    [[x,_,_],_,_] -> Three
    [[x,_],[y,_],_] -> TwoPair
    [[x,_],_,_,_] -> Pair
    [[x],_,_,_,_] -> HighCard
    otherwise -> error "Bam"
    where ys = reverse . sortOn (length) . group . sort $ xs

parseLine :: String -> (Hand, Int)
parseLine s = (ranks s1, read s2)
    where [s1, s2] = words s

solve :: [(Hand, Int)] -> Int
solve xs = sum . map (\(n,m) -> n * m) $ zip [x | (_,_,_,x) <- reverse ys''] [1..]
    where
        ys = map (\(x, y) -> (bestHand x, x, y)) xs
        ys' = map (\(x,y,z) -> (handType x, x, y, z)) ys
        ys'' = sortOn (\(x,_,_,_) -> x) . sortOn (\(_,_,x,_) -> x) $ ys'


main = do
    str <- getContents
    let xs = map parseLine $ lines str
    print $ solve xs
