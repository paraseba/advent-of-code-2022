module Utils where

import Control.Monad.Trans (MonadIO, liftIO)
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

parseAndSolveIO :: (MonadIO m) => Int -> Parser a -> (a -> m b) -> (b -> a -> m ()) -> m ()
parseAndSolveIO day parser solve1 solve2 = do
    input <- liftIO $ TIO.readFile fileName
    case parse parser fileName input of
        Left err -> liftIO $ putStrLn (errorBundlePretty err)
        Right d -> solve (solve1 d) (\prev -> solve2 prev d)
  where
    fileName = "./inputs/day" <> show day <> ".txt"

solve :: MonadIO m => m b -> (b -> m ()) -> m ()
solve solve1 solve2 = do
    liftIO $ putStrLn "=================   PART 1   ================="
    b <- solve1
    liftIO $ putStrLn "=================   PART 2   ================="
    solve2 b
    liftIO $ putStrLn "\n\nDone"

manhattan :: Num a => (a, a) -> (a, a) -> a
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
