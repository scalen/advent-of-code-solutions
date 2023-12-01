import System.Environment

findDigits :: String -> String
findDigits []       = []
findDigits ('o':cs) = case cs of
                        ('n':'e':_)         -> '1':findDigits cs
                        _                   ->     findDigits cs
findDigits ('t':cs) = case cs of
                        ('w':'o':_)         -> '2':findDigits cs
                        ('h':'r':'e':'e':_) -> '3':findDigits cs
                        _                   ->     findDigits cs
findDigits ('f':cs) = case cs of
                        ('o':'u':'r':_)     -> '4':findDigits cs
                        ('i':'v':'e':_)     -> '5':findDigits cs
                        _                   ->     findDigits cs
findDigits ('s':cs) = case cs of
                        ('i':'x':_)         -> '6':findDigits cs
                        ('e':'v':'e':'n':_) -> '7':findDigits cs
                        _                   ->     findDigits cs
findDigits ('e':cs) = case cs of
                        ('i':'g':'h':'t':_) -> '8':findDigits cs
                        _                   ->     findDigits cs
findDigits ('n':cs) = case cs of
                        ('i':'n':'e':_)     -> '9':findDigits cs
                        _                   ->     findDigits cs
findDigits (c:cs) |    '0' <= c && c <= '9'  =  c :findDigits cs
                  |               otherwise  =     findDigits cs

findDigitChars :: String -> String
findDigitChars line = [c | c <- line, '0' <= c, c <= '9']

parseCallibrationValue :: (String -> String) -> String -> Int
parseCallibrationValue digitFinder = read.firstAndLast.digitFinder
  where
    firstAndLast []     = ['0']
    firstAndLast (x:xs) = x:last(x:xs)
    last []     = []
    last [x]    = [x]
    last (_:xs) = last xs

solve :: Char -> String -> Int
solve part = case part of
               '1' -> sum.(map $ parseCallibrationValue findDigitChars).lines
               '2' -> sum.(map $ parseCallibrationValue findDigits).lines

main :: IO ()
main = do
  args <- getArgs
  case args of
    [[part], inputFilepath] -> do
                         input <- readFile inputFilepath
                         print $ solve part input
    _               -> putStrLn "Wrong number of arguments\n\nUsage:\n  main INPUT_FILEPATH PART_TO_SOLVE"
