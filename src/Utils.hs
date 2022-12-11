module Utils where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, parse)

type Parser = Parsec Void T.Text


parseAndSolve :: (Show b, Show c) => Int -> Parser a -> (a -> b) -> (b -> a -> c) -> IO ()
parseAndSolve day parser solve1 solve2 =
    parseAndSolveIO day parser action1 action2
    where
        action1 a = do
            let b = solve1 a
            putStrLn . show $ b
            pure b
        action2 a b = putStrLn . show $ solve2 a b


parseAndSolveIO :: Int -> Parser a -> (a -> IO b) -> (b -> a -> IO ()) -> IO ()
parseAndSolveIO day parser solve1 solve2 = do
    input <- TIO.readFile fileName
    case parse parser fileName input of
         Left err -> putStrLn (errorBundlePretty err)
         Right d -> solve (solve1 d) (\prev -> solve2 prev d) 

    where fileName = "day" <> show day <> ".txt"

solve :: IO b -> (b -> IO ()) -> IO ()
solve solve1 solve2 = do
    putStrLn "=================   PART 1   ================="
    b <- solve1
    putStrLn "=================   PART 2   ================="
    solve2 b
    putStrLn "\n\nDone"
