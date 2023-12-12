import System.Environment
import qualified Data.List as List
import qualified Data.Map as Map

type Label = String
type Directions = (Label, Label)
type Instruction = Directions -> Label
type Graph = Map.Map Label Directions

-- Parsing

swallowSpace :: String -> String
swallowSpace (' ':cs) = swallowSpace cs
swallowSpace cs       = cs

parseLabel :: String -> (Label, String)
parseLabel "" = ("", "")
parseLabel (c:cs)
  | elem c ['A'..'Z'] || elem c ['0'..'9'] = (c:token, remainder)
  | otherwise                              = ("", swallowSpace (c:cs))
  where
    (token, remainder) = parseLabel cs

-- Day

parseNode :: String -> (Label, Directions)
parseNode line = (label, parseDestinations destinationsStr)
  where
    (label, destinationsStr) = parseLabel line
    parseDestinations ('=':rest) = parseDestinations $ swallowSpace rest
    parseDestinations ('(':destsStr) = (llabel, parseRightDest rlabelStr)
      where
        (llabel, rlabelStr) = parseLabel destsStr
        parseRightDest (',':rdestStr) = fst $ parseLabel $ swallowSpace rdestStr

parseInstructions :: String -> [Instruction]
parseInstructions ""       = []
parseInstructions ('L':cs) = fst:parseInstructions cs
parseInstructions ('R':cs) = snd:parseInstructions cs

-- Part 1

journeyLengthFromTo :: Graph -> [Instruction] -> [Label] -> (Label -> Int -> (Int, Label)) -> Label -> Int -> (Int, Label)
journeyLengthFromTo graph choices tos recurse from steps
  | elem from tos = (steps, from)
  | otherwise     = recurse (choose $ graph Map.! from) (steps + 1)
  where
    (choose:_) = drop (mod steps $ length choices) choices

prepareJourney :: Label -> Label -> [String] -> Int
prepareJourney from to (instructionStr:_:graphStrs) = fst $ recursiveJourney from 0
  where
    recursiveJourney = (\f -> let {y = f y} in y) (journeyLengthFromTo graph instructions [to])
    graph = Map.fromList $ map parseNode graphStrs
    instructions = parseInstructions instructionStr

-- Part 2

memoizeJourneyWalker :: [Label] -> Int -> (Label -> Int -> (Int, Label)) -> (Label -> Int -> (Int, Label))
memoizeJourneyWalker labels limit f = getJourneyFor
  where
    getJourneyFor l i
      | i < limit  = (journeys Map.! l) !! i
      | i >= limit = (((div i limit) * limit) + length, to)
      where
        (length, to) = (journeys Map.! l) !! (mod i limit)
    journeys = Map.fromList [(label, map (f label) [0..limit]) | label <- labels]

memoizedJourneyLengthFrom :: Graph -> [Instruction] -> [Label] -> (Label -> Int -> (Int, Label))
memoizedJourneyLengthFrom graph instructions tos = (\f -> let {y = f y} in y) (memoize . preparedJourneyWalker)
  where
    memoize = memoizeJourneyWalker (Map.keys graph) (length instructions)
    preparedJourneyWalker = journeyLengthFromTo graph instructions tos

ghostJourneyLengthFromTo :: Graph -> [Instruction] -> Char -> Char -> Int
ghostJourneyLengthFromTo graph instructions fromChar toChar = shortestCommonLength froms
  where
    getJourneyLength = memoizedJourneyLengthFrom graph instructions tos
    tos              = [label      | label <- Map.keys graph, (last label) == toChar]
    froms            = [(0, label) | label <- Map.keys graph, (last label) == fromChar]
    shortestCommonLength ((lengthSoFar, from):furtherFroms)
      | success   = lengthSoFar
      | otherwise = shortestCommonLength $ List.sort $ (getJourneyLength from lengthSoFar):furtherFroms
      where
        success = all ((lengthSoFar ==) . fst) furtherFroms && lengthSoFar > 0

prepareGhostJourney :: Char -> Char -> [String] -> Int
prepareGhostJourney from to (instructionStr:_:graphStrs) = ghostJourneyLengthFromTo graph instructions from to
  where
    graph = Map.fromList $ map parseNode graphStrs
    instructions = parseInstructions instructionStr

-- Boilerplate and solution entrypoints

data Part = One | Two deriving (Show, Ord, Eq, Enum, Bounded)
instance Read Part where
  readsPrec _ "1" = [(One, "")]
  readsPrec _ "2" = [(Two, "")]

solve :: Part -> String -> String
solve One = show . (prepareJourney "AAA" "ZZZ") . lines
solve Two = show . (prepareGhostJourney 'A' 'Z') . lines
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
