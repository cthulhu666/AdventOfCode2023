import Data.List (elemIndex, group, sort, sortOn)

data Card = CardA | CardK | CardQ | CardJ | CardT | Card9 | Card8 | Card7 | Card6 | Card5 | Card4 | Card3 | Card2
    deriving (Bounded,Enum,Eq,Ord,Show)

ranks :: [Char] -> [Card]
ranks xs =
  case f xs of
    Just ys -> map toEnum ys
    Nothing -> error "Bam"
  where
    f = sequence . map (\x -> elemIndex x cards)
    cards = ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2']

data HandType = Five | Four  | Full  | Three  | TwoPair  | Pair  | HighCard
    deriving (Eq, Ord, Show)

type Hand = [Card]

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
solve xs = sum . map (\(n,m) -> n * m) $ zip [x | (_,_,x) <- reverse ys'] [1..]
    where
        ys = map (\(x, y) -> (handType x, x, y)) xs
        ys' = sortOn (\(x,_,_) -> x) . sortOn (\(_,x,_) -> x) $ ys


main = do
    str <- getContents
    let xs = map parseLine $ lines str
    print $ solve xs
