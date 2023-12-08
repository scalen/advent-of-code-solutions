import System.Environment
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
parseInstructions = cycle . parse
  where
    parse ""       = []
    parse ('L':cs) = fst:parse cs
    parse ('R':cs) = snd:parse cs

-- Part 1

journeyLengthFromTo :: Graph -> [Instruction] -> Label -> Label -> Int
journeyLengthFromTo = takeStep 0
  where
    takeStep steps graph (choose:choices) from to
      | from == to = steps
      | otherwise  = takeStep (steps+1) graph choices (choose $ graph Map.! from) to

prepareJourney :: Label -> Label -> [String] -> Int
prepareJourney from to (instructionStr:_:graphStrs) = journeyLengthFromTo graph instructions from to
  where
    graph = Map.fromList $ map parseNode graphStrs
    instructions = parseInstructions instructionStr

-- Part 2

ghostJourneyLengthFromTo :: Graph -> [Instruction] -> Char -> Char -> Int
ghostJourneyLengthFromTo graph instructions fromChar toChar = takeSteps 0 graph instructions [label | label <- Map.keys graph, (last label) == fromChar] toChar
  where
    takeSteps :: Int -> Graph -> [Instruction] -> [String] -> Char -> Int
    takeSteps steps graph (choose:choices) from toChar
      | all ((== toChar) . last) from = steps
      | otherwise                     = takeSteps (steps+1) graph choices (map (choose . (graph Map.!)) from) toChar

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
