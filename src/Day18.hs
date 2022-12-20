{-# LANGUAGE RecordWildCards #-}

module Day18 (main) where

import Control.Lens
import Control.Monad.Combinators.NonEmpty (sepEndBy1)
import Data.Foldable (foldl', toList)
import Data.Ix (inRange)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils

cubesP :: Parser (NonEmpty (Int, Int, Int))
cubesP = sepEndBy1 coord eol
  where
    coord = (,,) <$> decimal <* char ',' <*> decimal <* char ',' <*> decimal

type Coord = (Int, Int, Int)

main :: IO ()
main = parseAndSolveIO 18 cubesP solve1 solve2

neighbors :: (Num a, Num b, Num c) => (a, b, c) -> [(a, b, c)]
neighbors (x, y, z) =
    [ (x - 1, y, z)
    , (x + 1, y, z)
    , (x, y - 1, z)
    , (x, y + 1, z)
    , (x, y, z - 1)
    , (x, y, z + 1)
    ]

countMissingNeighbors :: Set.Set Coord -> (Int, Int, Int) -> Int
countMissingNeighbors potential coord = length $ filter (== False) $ flip Set.member potential <$> neighbors coord

countNeighbors :: Set.Set Coord -> (Int, Int, Int) -> Int
countNeighbors potential coord = length $ filter (flip Set.member potential) (neighbors coord)

solve1 :: NonEmpty Coord -> IO (Set.Set Coord)
solve1 input = do
    let res = foldl' count 0 cubes
    print res
    pure cubes
  where
    cubes = Set.fromList (toList input)
    count res coord = res + countMissingNeighbors cubes coord

dfs ::
    Set.Set Coord ->
    ((Int, Int, Int), (Int, Int, Int)) ->
    Set.Set (Int, Int, Int) ->
    Seq.Seq (Int, Int, Int) ->
    Set.Set (Int, Int, Int) ->
    (Set.Set (Int, Int, Int), Set.Set (Int, Int, Int))
dfs cubes range visited pending found =
    case uncons pending of
        Just (next, morePending) ->
            let (inp, outp) =
                    neighbors next
                        & filter (inRange range)
                        & filter (flip Set.notMember visited)
                        & partition (flip Set.member cubes)
             in dfs cubes range (Set.insert next visited) (Seq.fromList outp <> morePending) (Set.fromList inp <> found)
        Nothing -> (found, visited)

solve2 :: Set.Set Coord -> p -> IO ()
solve2 cubes _ = do
    print $ foldl' count 0 border
  where
    range =
        (
            ( minimum1Of (folded . _1) cubes - 1
            , minimum1Of (folded . _2) cubes - 1
            , minimum1Of (folded . _3) cubes - 1
            )
        ,
            ( maximum1Of (folded . _1) cubes + 1
            , maximum1Of (folded . _2) cubes + 1
            , maximum1Of (folded . _3) cubes + 1
            )
        )
    start = range ^. _1
    (border, exterior) = dfs cubes range Set.empty (Seq.singleton start) Set.empty
    count res coord = res + countNeighbors (exterior) coord
