import System.Environment

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

parseSeedList :: String -> [Int]
parseSeedList ('s':'e':'e':'d':'s':':':cs) = numbers
  where
    (numbers, _) = parseNumberList cs

type Start = Int
type Size = Int
type RangeMapping = (Start, Start, Size)
type Map = [RangeMapping]

parseMap :: [String] -> (Map, [String])
parseMap []           = ([], [])
parseMap ("":lines)   = ([], lines)
parseMap (line:lines) = ((from, to, range):furtherMap, remainingLines)
  where
    ([to, from, range], _) = parseNumberList line
    (furtherMap, remainingLines) = parseMap lines

parseMaps :: [String] -> [Map]
parseMaps []         = []
parseMaps ("":lines) = parseMaps lines
parseMaps (_:lines)  = map:parseMaps remainder
  where
    (map, remainder) = parseMap lines

-- Part 1

mapOnce :: Int -> Map -> Int
mapOnce src []                    = src
mapOnce src ((from, to, range):rest)
  | offset >= 0 && offset < range = to + offset
  | otherwise                     = mapOnce src rest
  where
    offset = src - from

processLinesToLocationsForSeeds :: [String] -> [Int]
processLinesToLocationsForSeeds (line:lines) = map (\seed -> foldl mapOnce seed $ parseMaps lines) $ parseSeedList line

-- Part 2

type Range = (Start, Size)

parseSeedRangeList :: String -> [Range]
parseSeedRangeList = pairItems . parseSeedList
  where
    pairItems [] = []
    pairItems (a:b:rest) = (a, b):pairItems rest

mapRange :: Range -> Map -> [Range]
mapRange src [] = [src]
mapRange (srcStart, srcSize) ((from, to, mappingSize):rest)
  | subsumes (from, mappingSize) (srcStart, srcSize) = [(to + offset, srcSize)]
  | subsumes (srcStart, srcSize) (from, mappingSize) = mappedLowerRange ++ [(to, mappingSize)] ++ mappedHigherRange
  | overlaps (srcStart, srcSize) from                = mappedLowerRange ++ [(to, mappingSize + offset)]
  | overlaps (from, mappingSize) srcStart            = [(to + offset, srcSize - excess)] ++ mappedHigherRange
  | otherwise                                        = mapRange (srcStart, srcSize) rest
  where
    offset = srcStart - from
    excess = srcStart + srcSize - (from + mappingSize)
    subsumes (outStart, outSize) (inStart, inSize)
      | inStart + inSize >= outStart + outSize = False
      | otherwise                              = inStart >= outStart
    overlaps (lowerStart, lowerSize) target
      | target > lowerStart = lowerStart + lowerSize >= target
      | otherwise           = False
    mappedLowerRange = mapRange (srcStart, (-offset)) rest
    mappedHigherRange = mapRange (from + mappingSize, excess) rest

processLinesToLocationsForSeedRanges :: [String] -> [Range]
processLinesToLocationsForSeedRanges (line:lines) = foldl (++) [] $ map getLocationsForSeedRange $ parseSeedRangeList line
  where
    getLocationsForSeedRange range = foldl getLocationsForSeedRanges [range] $ parseMaps lines
    getLocationsForSeedRanges ranges m = foldl (++) [] $ map (\r -> mapRange r m) ranges

-- Boilerplate and solution entrypoints

data Part = One | Two deriving (Show, Ord, Eq, Enum, Bounded)
instance Read Part where
  readsPrec _ "1" = [(One, "")]
  readsPrec _ "2" = [(Two, "")]

solve :: Part -> String -> String
solve One = show . (foldl min maxBound) . processLinesToLocationsForSeeds . lines
solve Two = show . (foldl min maxBound) . (map fst) . processLinesToLocationsForSeedRanges . lines
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
