{-# LANGUAGE LambdaCase #-}

module Day2 where

import Text.Megaparsec.Char
import Text.Megaparsec hiding (sepBy1, sepEndBy1)
import Control.Applicative.Combinators.NonEmpty (sepEndBy1)
import Data.List.NonEmpty (NonEmpty)
import Control.Lens
import Utils (parseAndSolve, calculateBoth, Parser)

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


solve1 :: NonEmpty (Play, Play) -> Integer
solve1 = sumOf (traverse.to roundScore)

solve2 :: NonEmpty (Play, Play) -> Integer
solve2 = sumOf (traverse.to fix.to roundScore)
    where
        fix :: (Play, Play) -> (Play, Play)
        fix r = r & _2 .~ (fixInterpretation r)

main :: IO ()
main = parseAndSolve 2 guideP (calculateBoth solve1 solve2)
