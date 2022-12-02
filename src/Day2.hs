{-# LANGUAGE LambdaCase #-}

module Day2 where

import Text.Megaparsec.Char
import Text.Megaparsec hiding (sepBy1, sepEndBy1)
import Data.Void (Void)
import Data.Text qualified as T
import Control.Applicative.Combinators.NonEmpty (sepEndBy1)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text.IO as TIO
import Control.Lens

type Parser = Parsec Void T.Text

data Play = Rock | Paper | Scissors
    deriving (Show)

elfP :: Parser Play
elfP = oneOf ['A'..'C'] >>= \case
    'A' -> pure Rock
    'B' -> pure Paper
    'C' -> pure Scissors
    _ -> empty

meP :: Parser Play
meP = oneOf ['X'..'Z'] >>= \case
    'X' -> pure Rock
    'Y' -> pure Paper
    'Z' -> pure Scissors
    _ -> empty

gameRoundP :: Parser (Play, Play)
gameRoundP = (,) <$> (elfP <* char ' ') <*> meP

guideP :: Parser (NonEmpty (Play, Play))
guideP = (gameRoundP `sepEndBy1` eol) <* eof

shapeScore :: Play -> Integer
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

resultScore :: (Play, Play) -> Integer
resultScore (Rock, Paper) = 6
resultScore (Rock, Scissors) = 0
resultScore (Rock, Rock) = 3
resultScore (Paper, Scissors) = 6
resultScore (Paper, Rock) = 0
resultScore (Paper, Paper) = 3
resultScore (Scissors, Rock) = 6
resultScore (Scissors, Paper) = 0
resultScore (Scissors, Scissors) = 3

roundScore :: (Play, Play) -> Integer
roundScore (elf, me) = shapeScore me + resultScore (elf, me)

fixInterpretation :: (Play, Play) -> Play
fixInterpretation (Rock, Paper) = Rock
fixInterpretation (Rock, Scissors) = Paper
fixInterpretation (Rock, Rock) = Scissors
fixInterpretation (Paper, Scissors) = Scissors
fixInterpretation (Paper, Rock) = Rock
fixInterpretation (Paper, Paper) = Paper
fixInterpretation (Scissors, Rock) = Paper
fixInterpretation (Scissors, Paper) = Scissors
fixInterpretation (Scissors, Scissors) = Rock

main :: IO ()
main = do
    input <- TIO.readFile "day2.txt"
    case parse guideP "day2.txt" input of
         Left err -> putStrLn (errorBundlePretty err)
         Right g -> do
            print $ calculate1 g
            print $ calculate2 g
    putStrLn "Done"

calculate1 :: NonEmpty (Play, Play) -> Integer
calculate1 = sumOf (traverse.to roundScore)

calculate2 :: NonEmpty (Play, Play) -> Integer
calculate2 = sumOf (traverse.to fix.to roundScore)
    where
        fix :: (Play, Play) -> (Play, Play)
        fix r = r & _2 .~ (fixInterpretation r)

