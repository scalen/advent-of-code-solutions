import System.Environment
import qualified Data.List as List

-- Number parsing

swallowSpace :: String -> String
swallowSpace (' ':cs) = swallowSpace cs
swallowSpace cs       = cs

parseToken :: (Char -> Bool) -> String -> (String, String)
parseToken _ "" = ("", "")
parseToken condition (c:cs)
  | condition c = (c:token, remainder)
  | otherwise   = ("", swallowSpace (c:cs))
  where
    (token, remainder) = parseToken condition cs

parseNumber :: String -> (Int, String)
parseNumber line = (read numberStr, remainder)
  where
    (numberStr, remainder) = parseToken (\c -> '0' <= c && c <= '9') $ swallowSpace line

-- Day

type Bid = Int
data Card = Joker | Tw | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Read, Eq, Ord, Enum, Bounded)

data HandType = EmptyHand | HighCard Card | OnePair Card | TwoPair Card Card | ThreeOfAKind Card | FullHouse Card Card | FourOfAKind Card | FiveOfAKind Card
instance Eq HandType where
  EmptyHand        == EmptyHand        = True
  (HighCard _)     == (HighCard _)     = True
  (OnePair _)      == (OnePair _)      = True
  (TwoPair _ _)    == (TwoPair _ _)    = True
  (ThreeOfAKind _) == (ThreeOfAKind _) = True
  (FullHouse _ _)  == (FullHouse _ _)  = True
  (FourOfAKind _)  == (FourOfAKind _)  = True
  (FiveOfAKind _)  == (FiveOfAKind _)  = True
  _                == _                = False
instance Ord HandType where
  EmptyHand        <= _                = True
  _                <= EmptyHand        = False
  (HighCard _)     <= _                = True
  _                <= (HighCard _)     = False
  (OnePair _)      <= _                = True
  _                <= (OnePair _)      = False
  (TwoPair _ _)    <= _                = True
  _                <= (TwoPair _ _)    = False
  (ThreeOfAKind _) <= _                = True
  _                <= (ThreeOfAKind _) = False
  (FullHouse _ _)  <= _                = True
  _                <= (FullHouse _ _)  = False
  (FourOfAKind _)  <= _                = True
  _                <= (FourOfAKind _)  = False
  (FiveOfAKind _)  <= _                = True
type Hand = (HandType, [Card])
type Play = (Hand, Bid)

parsePlay :: (String -> ([Card], String)) -> ([Card] -> HandType) -> String -> Play
parsePlay parseCards determineHandType line = ((determineHandType cards, cards), fst $ parseNumber remainder)
  where
    (cards, remainder) = parseCards line

type Rank = Int

rankPlays :: [Play] -> [(Rank, Bid)]
rankPlays = (rank 1) . (List.sortBy beats)
  where
    rank _ []               = []
    rank r ((_, bid):plays) = (r, bid):rank (r+1) plays
    beats (hand1, _) (hand2, _) = compare hand1 hand2

scoreRankedPlay :: (Rank, Bid) -> Int
scoreRankedPlay (rank, bid) = rank * bid

-- Part 1

parseCards :: String -> ([Card], String)
parseCards line = (readCards token, remainder)
  where
    (token, remainder) = parseToken (\c -> elem c "AKQJT98765432") line
    readCards ""         = []
    readCards ('A':rest) = Ace  :readCards rest
    readCards ('K':rest) = King :readCards rest
    readCards ('Q':rest) = Queen:readCards rest
    readCards ('J':rest) = Jack :readCards rest
    readCards ('T':rest) = Ten  :readCards rest
    readCards ('9':rest) = Nine :readCards rest
    readCards ('8':rest) = Eight:readCards rest
    readCards ('7':rest) = Seven:readCards rest
    readCards ('6':rest) = Six  :readCards rest
    readCards ('5':rest) = Five :readCards rest
    readCards ('4':rest) = Four :readCards rest
    readCards ('3':rest) = Three:readCards rest
    readCards ('2':rest) = Tw   :readCards rest

determineHandType :: [Card] -> HandType
determineHandType = recurse EmptyHand
  where
    recurse :: HandType -> [Card] -> HandType
    recurse EmptyHand (c:cs)
      | length (filter (c ==) cs) == 1 = recurse (OnePair c) (filter (c /=) cs)
      | length (filter (c ==) cs) == 2 = recurse (ThreeOfAKind c) (filter (c /=) cs)
      | length (filter (c ==) cs) == 3 = FourOfAKind c
      | length (filter (c ==) cs) == 4 = FiveOfAKind c
      | otherwise                      = recurse (HighCard c) cs
    recurse (HighCard h) (c:cs)
      | length (filter (c ==) cs) == 1 = recurse (OnePair c) (filter (c /=) cs)
      | length (filter (c ==) cs) == 2 = recurse (ThreeOfAKind c) (filter (c /=) cs)
      | length (filter (c ==) cs) == 3 = FourOfAKind c
      | c > h                          = recurse (HighCard c) cs
      | otherwise                      = recurse (HighCard h) cs
    recurse (OnePair p) (c:cs)
      | length (filter (c ==) cs) == 1 = TwoPair p c
      | length (filter (c ==) cs) == 2 = FullHouse c p
      | otherwise                      = recurse (OnePair p) cs
    recurse (ThreeOfAKind t) (c:cs)
      | length (filter (c ==) cs) == 1 = FullHouse t c
      | otherwise                      = ThreeOfAKind t
    recurse acc _                      = acc

-- Part 2

parseCardsWithJoker :: String -> ([Card], String)
parseCardsWithJoker line = (readCards token, remainder)
  where
    (token, remainder) = parseToken (\c -> elem c "AKQT98765432J") line
    readCards ""         = []
    readCards ('A':rest) = Ace  :readCards rest
    readCards ('K':rest) = King :readCards rest
    readCards ('Q':rest) = Queen:readCards rest
    readCards ('T':rest) = Ten  :readCards rest
    readCards ('9':rest) = Nine :readCards rest
    readCards ('8':rest) = Eight:readCards rest
    readCards ('7':rest) = Seven:readCards rest
    readCards ('6':rest) = Six  :readCards rest
    readCards ('5':rest) = Five :readCards rest
    readCards ('4':rest) = Four :readCards rest
    readCards ('3':rest) = Three:readCards rest
    readCards ('2':rest) = Tw   :readCards rest
    readCards ('J':rest) = Joker:readCards rest

determineHandTypeWithJoker :: [Card] -> HandType
determineHandTypeWithJoker = recurse EmptyHand 0
  where
    recurse :: HandType -> Int -> [Card] -> HandType
    recurse acc              0      []         = acc
    recurse EmptyHand        5      []         = FiveOfAKind Joker
    recurse (HighCard h)     jokers []         = case jokers of
                                                   1 -> OnePair h
                                                   2 -> ThreeOfAKind h
                                                   3 -> FourOfAKind h
                                                   4 -> FiveOfAKind h
    recurse (OnePair h)      jokers []         = case jokers of
                                                   1 -> ThreeOfAKind h
                                                   2 -> FourOfAKind h
                                                   3 -> FiveOfAKind h
    recurse (TwoPair h l)    jokers []         = case jokers of
                                                   1 -> FullHouse h l
    recurse (ThreeOfAKind h) jokers []         = case jokers of
                                                   1 -> FourOfAKind h
                                                   2 -> FiveOfAKind h
    recurse (FourOfAKind h)  jokers []         = case jokers of
                                                   1 -> FiveOfAKind h
    recurse acc              jokers (Joker:cs) = recurse acc (jokers + 1) cs
    recurse EmptyHand        jokers (c:cs)
      | length (filter (c ==) cs) == 1         = recurse (OnePair c) jokers (filter (c /=) cs)
      | length (filter (c ==) cs) == 2         = recurse (ThreeOfAKind c) jokers (filter (c /=) cs)
      | length (filter (c ==) cs) == 3         = recurse (FourOfAKind c) jokers cs
      | length (filter (c ==) cs) == 4         = FiveOfAKind c
      | otherwise                              = recurse (HighCard c) jokers cs
    recurse (HighCard h)     jokers (c:cs)
      | length (filter (c ==) cs) == 1         = recurse (OnePair c) jokers (filter (c /=) cs)
      | length (filter (c ==) cs) == 2         = recurse (ThreeOfAKind c) jokers (filter (c /=) cs)
      | length (filter (c ==) cs) == 3         = FourOfAKind c
      | c > h                                  = recurse (HighCard c) jokers cs
      | otherwise                              = recurse (HighCard h) jokers cs
    recurse (OnePair p)      jokers (c:cs)
      | length (filter (c ==) cs) == 1         = recurse (TwoPair p c) jokers cs
      | length (filter (c ==) cs) == 2         = FullHouse c p
      | otherwise                              = recurse (OnePair p) jokers cs
    recurse (ThreeOfAKind t) jokers (c:cs)
      | length (filter (c ==) cs) == 1         = FullHouse t c
      | otherwise                              = recurse (ThreeOfAKind t) jokers cs
    recurse acc              jokers (c:cs)     = recurse acc jokers cs

-- Boilerplate and solution entrypoints

data Part = One | Two deriving (Show, Ord, Eq, Enum, Bounded)
instance Read Part where
  readsPrec _ "1" = [(One, "")]
  readsPrec _ "2" = [(Two, "")]

solve :: Part -> String -> String
solve One = show . sum . (map scoreRankedPlay) . rankPlays . (map (parsePlay parseCards determineHandType)) . lines
solve Two = show . sum . (map scoreRankedPlay) . rankPlays . (map (parsePlay parseCardsWithJoker determineHandTypeWithJoker)) . lines
solve _ = \_ -> "Unsolved"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [part, inputFilepath] -> do
                              input <- readFile inputFilepath
                              putStrLn $ solve (read part) input
    [inputFilepath]      -> do
                              input <- readFile inputFilepath
                              mapM_ putStrLn $ map (\p -> "Part " ++ (show p) ++ ": " ++ (solve p input)) ([minBound..maxBound] :: [Part])
    _                    -> putStrLn "Wrong number of arguments\n\nUsage:\n  main [ 1 | 2 ] INPUT_FILEPATH"
