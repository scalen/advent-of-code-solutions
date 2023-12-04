import System.Environment

-- General

type PartLocation = (Int, Either (Maybe (Int -> Int)) (Maybe Int))
type Scope = [Int]
type PartNumber = (Int, Scope)

isPart c = ('0' > c || c > '9') && c /= '.'

getEnginePartLocations :: (Char -> Bool) -> String -> [PartLocation]
getEnginePartLocations = finder 0
  where
    finder _ _ "" = []
    finder index matcher (c:cs)
      | matcher c = (index, Left Nothing):finder (index + 1) matcher cs
      | otherwise =       finder (index + 1) matcher cs

getPossibleEnginePartNumbersAndLocations :: String -> [PartNumber]
getPossibleEnginePartNumbersAndLocations = finder 0
  where
    finder :: Int -> String -> [PartNumber]
    finder _ "" = []
    finder index (c:cs)
      | '0' <= c && c <= '9' = (read token, scope):finder lastIndex rest
      | otherwise            = finder (index + 1) cs
      where
        (token, scope, rest, lastIndex) = parser index (c:cs)
    parser :: Int -> String -> (String, Scope, String, Int)
    parser index ""          = ("", [index - 1, index], "", index)
    parser index (c:cs)
      | '0' <= c && c <= '9' = (c:token, (index - 1):scope, rest, lastIndex)
      | otherwise            = ("", [index-1, index], c:cs, index)
      where
        (token, scope, rest, lastIndex) = parser (index + 1) cs

-- Part 1

getPartNumbers1D :: [PartLocation] -> [[PartNumber]] -> ([Int], [[PartNumber]])
getPartNumbers1D partLocations []             = ([], [])
getPartNumbers1D partLocations (partNos:rows) = (poppedNumbers ++ recNumbers, remainingPartNos:remainingRowsContents)
  where
    (poppedNumbers, remainingPartNos) = popNumbersForParts partLocations partNos
    (recNumbers, remainingRowsContents) = getPartNumbers1D partLocations rows
    popNumbersForParts :: [PartLocation] -> [PartNumber] -> ([Int], [PartNumber])
    popNumbersForParts partLocations []       = ([], [])
    popNumbersForParts partLocations ((number, scope):partNos)
      | partExistsInScope partLocations scope = (number:numbers, remainingPartNos)
      | otherwise                             = (numbers, (number, scope):remainingPartNos)
      where
        (numbers, remainingPartNos) = popNumbersForParts partLocations partNos
    partExistsInScope :: [PartLocation] -> Scope -> Bool
    partExistsInScope [] scope = False
    partExistsInScope ((loc, _):partLocations) scope
      | elem loc scope = True
      | otherwise      = partExistsInScope partLocations scope

getPartNumbers2D :: [[PartLocation]] -> [[PartNumber]] -> [Int]
getPartNumbers2D = iterator []
  where
    iterator :: [[PartNumber]] -> [[PartLocation]] -> [[PartNumber]] -> [Int]
    iterator [] _ []                            = []
    iterator _ [] _                             = []
    iterator [] plrs (row:rows)                 = iterator [[], [], row] plrs rows
    iterator (_:previous) (pls:plrs) []         = matchedNumbers ++ (iterator remainingRowsContents plrs [])
      where
        (matchedNumbers, remainingRowsContents) = getPartNumbers1D pls previous
    iterator (_:previous) (pls:plrs) (row:rows) = matchedNumbers ++ (iterator remainingRowsContents plrs rows)
      where
        (matchedNumbers, remainingRowsContents) = getPartNumbers1D pls (previous ++ [row])

getPartNumbersFromString :: String -> [Int]
getPartNumbersFromString "" = []
getPartNumbersFromString input = getPartNumbers2D partLocations partNumbers
  where
    partLocations = map (getEnginePartLocations isPart) splitLines
    partNumbers = map getPossibleEnginePartNumbersAndLocations splitLines
    splitLines = lines input

-- Part 2

getGearRatios1D :: [PartLocation] -> [[PartNumber]] -> [Int]
getGearRatios1D partLocations []         = extractRatios partLocations
  where
    extractRatios []                                      = []
    extractRatios ((_, Right (Just ratio)):partLocations) = ratio:extractRatios partLocations
    extractRatios (_:partLocations)                       =       extractRatios partLocations
getGearRatios1D partLocations (row:rows) = getGearRatios1D (foldl (updateGearStates) partLocations [partNo | partNo <- row]) rows
  where
    updateGearStates :: [PartLocation] -> PartNumber -> [PartLocation]
    updateGearStates [] _ = []
    updateGearStates ((loc, state):partLocations) (number, scope)
      | elem loc scope    = case state of
          Left Nothing          -> (loc, Left (Just (number *))):updateGearStates partLocations (number, scope)
          Left (Just completer) -> (loc, Right (Just (completer number))):updateGearStates partLocations (number, scope)
          _                     -> (loc, Right Nothing):updateGearStates partLocations (number, scope) -- If the part has already produced a result, it must have more that two adjacent part numbers, so we should move it to an invalid gear state
      | otherwise         = (loc, state):updateGearStates partLocations (number, scope)

getGearRatios2D :: [[PartLocation]] -> [[PartNumber]] -> [Int]
getGearRatios2D = iterator []
  where
    iterator :: [[PartNumber]] -> [[PartLocation]] -> [[PartNumber]] -> [Int]
    iterator [] _ []                            = []
    iterator _ [] _                             = []
    iterator [] plrs (row:rows)                 = iterator [[], [], row] plrs rows
    iterator (_:previous) (pls:plrs) []         = getGearRatios1D pls previous
    iterator (_:previous) (pls:plrs) (row:rows) = (getGearRatios1D pls next) ++ (iterator next plrs rows)
      where
        next = previous ++ [row]

getGearRatiosFromString :: String -> [Int]
getGearRatiosFromString "" = []
getGearRatiosFromString input = getGearRatios2D partLocations partNumbers
  where
    partLocations = map (getEnginePartLocations isPart) splitLines
    partNumbers = map getPossibleEnginePartNumbersAndLocations splitLines
    splitLines = lines input

-- Boiler plate and solution calls

data Part = One | Two deriving (Show, Ord, Eq, Enum, Bounded)
instance Read Part where
  readsPrec _ "1" = [(One, "")]
  readsPrec _ "2" = [(Two, "")]

solve :: Part -> String -> String
solve One = show . sum . getPartNumbersFromString
solve Two = show . sum . getGearRatiosFromString

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
