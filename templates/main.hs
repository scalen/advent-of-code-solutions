import System.Environment

solve1 :: String -> ...
solve2 :: String -> ...

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["1", inputFilepath] -> do
                              input <- readFile inputFilepath
                              print $ solve1 input
    ["2", inputFilepath] -> do
                              input <- readFile inputFilepath
                              print $ solve2 input
    [inputFilepath]      -> do
                              input <- readFile inputFilepath
                              putStr "Part 1: "
                              print $ solve1 input
                              putStr "Part 2: "
                              print $ solve2 input
    _                    -> putStrLn "Wrong number of arguments\n\nUsage:\n  main [ 1 | 2 ] INPUT_FILEPATH"
