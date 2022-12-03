{-# LANGUAGE ImpredicativeTypes #-}

module Day3 where

import Control.Applicative.Combinators.NonEmpty (sepEndBy1)
import Control.Lens
import Data.Char (isLower, ord)
import Data.Foldable (toList)
import Data.List.NonEmpty (some1)
import Data.Sequence qualified as Seq
import Data.Sequence.Lens (slicedFrom, slicedTo)
import Data.Set qualified as Set
import Data.Set.Lens (setOf)
import Text.Megaparsec.Char (eol, letterChar)
import Utils (Parser, calculateBoth, parseAndSolve)

type Contents = Seq.Seq Char

rucksack :: Parser Contents
rucksack = Seq.fromList . toList <$> some1 letterChar

rucksacks :: Parser (Seq.Seq Contents)
rucksacks = Seq.fromList . toList <$> sepEndBy1 rucksack eol

split :: Contents -> (Traversal' (Seq.Seq Char) Char, Traversal' (Seq.Seq Char) Char)
split c = (slicedTo middle, slicedFrom middle)
  where
    middle = length c `div` 2

priority :: Char -> Int
priority c =
    if isLower c
        then ord c - ord 'a' + 1
        else ord c - ord 'A' + 27

score :: Seq.Seq Char -> Int
score contents =
    let (t1, t2) = split contents
        c = (setOf t1 contents `Set.intersection` setOf t2 contents) ^? folded
     in maybe 0 priority c

main :: IO ()
main = parseAndSolve 3 rucksacks (calculateBoth solve1 solve2)

solve1 :: Seq.Seq Contents -> Int
solve1 = sumOf (folded . to score)

solve2 :: Seq.Seq Contents -> Int
solve2 packs =
    packs
        & Seq.chunksOf 3
        & over (mapped . mapped) (setOf folded)
        & over mapped (foldr1 Set.intersection)
        & over mapped (maybe 0 priority . (^? folded))
        & sumOf folded
