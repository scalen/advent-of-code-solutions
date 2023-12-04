import System.Environment

swallowSpace :: String -> String
swallowSpace (' ':cs) = swallowSpace cs
swallowSpace cs       = cs

parseToken :: (Char -> Bool) -> String -> (String, String)
parseToken _ "" = ("", "")
parseToken condition (c:cs)
  | condition c = case (parseToken condition cs) of
                    (token, remainder) -> ((c:token), remainder)
  | otherwise   = ("", swallowSpace (c:cs))

type Colour = String
type Selection = [(Colour, Int)]

parseColour :: String -> (Colour, String)
parseColour = (parseToken (\c -> ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))) . swallowSpace

parseNumber :: String -> (Int, String)
parseNumber line = case (parseToken (\c -> '0' <= c && c <= '9') $ swallowSpace line) of
                     (numberStr, remainder) -> ((read numberStr), remainder)

parseSelection :: String -> (Selection, String)
parseSelection ""       = ([], "")
parseSelection (';':cs) = ([], cs)
parseSelection (',':cs) = parseSelection cs
parseSelection line     = case (parseNumber line) of
                            (number, next) -> case (parseColour next) of
                                                (colour, next2) -> case (parseSelection next2) of
                                                                     (selection, remainder) -> (((colour, number):selection), remainder)

data Game = Game {
    gameId :: Int
  , selections :: [Selection]
  } deriving (Eq, Show)
instance Read Game where
  readsPrec _ ('G':'a':'m':'e':' ':cs) = case (parseNumber cs) of
                                           (number, ':':remainder) -> [(Game {gameId=number, selections=(parseSelections remainder)}, "")]
    where
      parseSelections "" = []
      parseSelections cs = case (parseSelection cs) of
                           (selection, remainder) -> selection:(parseSelections remainder)

minimalKnownSet :: [Selection] -> Selection
minimalKnownSet = minimalKnownSet' []
  where
    minimalKnownSet' acc []             = acc
    minimalKnownSet' acc ([]:ss)        = minimalKnownSet' acc ss
    minimalKnownSet' acc ((entry:s):ss) = minimalKnownSet' (maybeIncreaseMinimum entry acc) (s:ss)
    maybeIncreaseMinimum entry [] = [entry]
    maybeIncreaseMinimum (entryColour, entryCount) ((sColour, sCount):selection)
      | entryColour == sColour    = (sColour, if entryCount > sCount then entryCount else sCount):selection
      | otherwise                 = (sColour, sCount):(maybeIncreaseMinimum (entryColour, entryCount) selection)

inLimits :: Selection -> Game -> Bool
inLimits [] _                                            = False
inLimits _ (Game {gameId=_, selections=[]})              = True
inLimits limits (Game {gameId=_, selections=selections}) = not $ foldr (||) False $ map (hasBiggerCount (minimalKnownSet selections)) limits
  where
    hasBiggerCount [] _    = False
    hasBiggerCount ((sColour, count):selection) (lColour, maximum)
      | sColour == lColour = count > maximum
      | otherwise          = hasBiggerCount selection (lColour, maximum)

getPossibleGameId :: Selection -> Game -> Int
getPossibleGameId limits game
  | inLimits limits game             = gameId game
  | otherwise                        = 0

data Part = One | Two deriving (Show, Ord, Eq, Enum, Bounded)
instance Read Part where
  readsPrec _ "1" = [(One, "")]
  readsPrec _ "2" = [(Two, "")]

solve :: Part -> String -> String
solve One = show . sum . (map (getPossibleGameId [("red", 12), ("green", 13), ("blue", 14)])) . (map read) . lines
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
