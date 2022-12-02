module Utils where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, parse)

type Parser = Parsec Void T.Text

parseAndSolve :: Int -> Parser a -> (a -> IO b) -> IO ()
parseAndSolve day parser calculate = do
    input <- TIO.readFile fileName
    case parse parser fileName input of
         Left err -> putStrLn (errorBundlePretty err)
         Right d -> calculate d *> putStrLn "Done"

    where fileName = "day" <> show day <> ".txt"

calculateBoth :: (Show b, Show c) => (a -> b) -> (a -> c) -> a -> IO ()
calculateBoth f g a = do
    putStrLn $ "Part 1 result: " <> show (f a)
    putStrLn $ "Part 2 result: " <> show (g a)
