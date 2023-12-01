import System.Environment

solve :: Char -> String -> ...
solve part = case part of
               '1' -> ...
               '2' -> ...

main :: IO ()
main = do
  args <- getArgs
  case args of
    [[part], inputFilepath] -> do
                         input <- readFile inputFilepath
                         print $ solve part input
    _               -> putStrLn "Wrong number of arguments\n\nUsage:\n  main INPUT_FILEPATH PART_TO_SOLVE"
