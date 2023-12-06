import System.Environment

-- Parsing Numbers

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
parseNumberList ""   = ([], "")
parseNumberList line = (number:numbers, remainder)
  where
    (number, rest) = parseNumber line
    (numbers, remainder) = parseNumberList rest

-- Day

type Time = Int
type Distance = Int

parseTimes :: String -> [Time]
parseTimes ('T':'i':'m':'e':':':cs) = fst $ parseNumberList cs

parseDistances :: String -> [Distance]
parseDistances ('D':'i':'s':'t':'a':'n':'c':'e':':':cs) = fst $ parseNumberList cs

type RaceRecord = (Time, Distance)

parseRaceRecords :: [String] -> [RaceRecord]
parseRaceRecords [timeLine, distanceLine] = zip (parseTimes timeLine) (parseDistances distanceLine)

countPossibleNewRecords :: RaceRecord -> Int
countPossibleNewRecords (length, record) = length - 1 - recordHoldFloor * 2
  where
    -- The calculation of distance travelled in a race is d = h*(l-h), which is
    -- the quadratic equation 0 = h^2 - lh + 2, which we can solve for h using
    -- the quadratic formula. We can therefore calculate the two possible hold
    -- times that would have resulted in the record.
    -- We can then arbitrarily take the lower possible value, as the result is
    -- symetrical with regard to time from the nearest end of the race duration;
    -- the floor of that is the maximum number of discreet seconds, from either
    -- end of the race, on which releasing the button would result in failing to
    -- beat the record.
    -- N.B.: Int x - Ceiling (Float y) = Floor (Float x - Float y)
    recordHoldFloor = div (length - (ceiling $ sqrt $ fromIntegral $ length * length - 4 * record)) 2

-- Boilerplate

data Part = One | Two deriving (Show, Ord, Eq, Enum, Bounded)
instance Read Part where
  readsPrec _ "1" = [(One, "")]
  readsPrec _ "2" = [(Two, "")]

solve :: Part -> String -> String
solve One = show . (foldl (*) 1) . (map countPossibleNewRecords) . parseRaceRecords . lines
solve Two = show . (map countPossibleNewRecords) . parseRaceRecords . lines
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
