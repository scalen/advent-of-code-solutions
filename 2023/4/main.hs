import System.Environment

import qualified Data.Bits as Bits

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

parseNumberList :: String -> ([Int], String)
parseNumberList ""       = ([], "")
parseNumberList ('|':cs) = ([], cs)
parseNumberList line     = (number:numbers, lineRemainder)
  where
    (number, listRemainder) = parseNumber line
    (numbers, lineRemainder) = parseNumberList listRemainder

data Card = Card Int [Int] [Int]

parseCard :: String -> Card
parseCard ('C':'a':'r':'d':cs) = Card cardId winners gotNos
  where
    (cardId, ':':cardBody) = parseNumber cs
    (winners, remainder) = parseNumberList cardBody
    (gotNos, _) = parseNumberList remainder

getCardMatches :: Card -> Int
getCardMatches (Card _ winners gotNos) = sum [1 | n <- gotNos, elem n winners]

-- Part 1

getCardPoints :: Card -> Int
getCardPoints card = Bits.shift 1 $ (getCardMatches card) - 1

-- Part 2

getCardCounts :: [Card] -> [Int]
getCardCounts = getCardCountsDoubling []
  where
    getCardCountsDoubling _ [] = []
    getCardCountsDoubling doubles (c:cs) = noOfCard:(getCardCountsDoubling (nextDoubles) cs)
      where
        nextDoubles
          | noOfMatches > 0 = decrementedDoubles ++ (take noOfCard $ repeat noOfMatches)
          | otherwise       = decrementedDoubles
        decrementedDoubles = [d' | d <- doubles, let d' = d - 1, d' > 0]
        noOfMatches = getCardMatches c
        noOfCard = (length doubles) + 1

-- Boilerplate and solution entrypoints

data Part = One | Two deriving (Show, Ord, Eq, Enum, Bounded)
instance Read Part where
  readsPrec _ "1" = [(One, "")]
  readsPrec _ "2" = [(Two, "")]

solve :: Part -> String -> String
solve One = show . sum . (map (getCardPoints . parseCard)) . lines
solve Two = show . sum . getCardCounts . (map parseCard) . lines
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
