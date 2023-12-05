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

mapOnce :: Int -> Map -> Int
mapOnce src []                    = src
mapOnce src ((from, to, range):rest)
  | offset >= 0 && offset < range = to + offset
  | otherwise                     = mapOnce src rest
  where
    offset = src - from

processLinesToLocationsForSeeds :: [String] -> [Int]
processLinesToLocationsForSeeds (line:lines) = map (\seed -> foldl mapOnce seed $ parseMaps lines) $ parseSeedList line

-- Boilerplate and solution entrypoints

data Part = One | Two deriving (Show, Ord, Eq, Enum, Bounded)
instance Read Part where
  readsPrec _ "1" = [(One, "")]
  readsPrec _ "2" = [(Two, "")]

solve :: Part -> String -> String
solve One = show . (foldl min maxBound) . processLinesToLocationsForSeeds . lines
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
