module Day12 (main) where

import Algorithm.Search (bfs)
import Control.Lens hiding ((<|))
import Data.Char (ord, isLetter)
import Utils
import Vec2d
import Data.List.NonEmpty (NonEmpty)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec (takeWhileP)
import Data.Text (Text, unpack)
import Control.Applicative.Combinators.NonEmpty (sepEndBy1)
import Data.Foldable (toList)

data Size = Size Row Col
    deriving (Show)

data Point = Regular Coord | Exit Coord
    deriving (Eq, Ord, Show)

type HeightMap = Vec2d Char

neighbors :: Size -> Coord -> [Coord]
neighbors (Size nRows nCols) (row, col) =
    filter
        valid
        [ (row + 1, col)
        , (row - 1, col)
        , (row, col + 1)
        , (row, col - 1)
        ]
  where
    valid (r, c) = r >= 0 && c >= 0 && r < nRows && c < nCols

accessible :: HeightMap -> Size -> Coord -> (Int -> Int -> Bool) -> [Point]
accessible hm s current heightCondition =
    [ p | candidate <- (neighbors s current), let ch = hm ! candidate, let p = toPoint ch candidate, canGet ch
    ]
  where
    currentH = height (hm ! current)

    canGet :: Char -> Bool
    canGet ch = heightCondition currentH (height ch) -- height ch - currentH <= 1
    toPoint :: Char -> Coord -> Point
    toPoint 'E' coord = Exit coord
    toPoint _ coord = Regular coord

    height :: Char -> Int
    height 'E' = (ord 'z') - (ord 'a')
    height 'S' = 0
    height other = (ord other) - (ord 'a')

findChar :: Char -> HeightMap -> [Coord]
findChar c hm = fst <$> itoListOf (itraversed . filtered (== c)) hm

search :: HeightMap -> Point -> (Point -> Bool) -> (Int -> Int -> Bool) -> Maybe [Point]
search hm initial isExit heightCondition = bfs neighs isExit initial
  where
    size = Size (Row . numRows $ hm) (Col . numCols $ hm)
    neighs p = accessible hm size (point2coord p) heightCondition

point2coord :: Point -> Coord
point2coord (Regular c) = c
point2coord (Exit c) = c

mapP :: Parser (NonEmpty Text)
mapP = sepEndBy1 lineP eol
  where
    lineP :: Parser Text
    lineP = takeWhileP Nothing isLetter

main :: IO ()
main = parseAndSolve 12 mapP solve1 solve2

solve1 :: NonEmpty Text -> Maybe Int
solve1 input =
    length <$> search hm (Regular startCoord) isExit heightCondition
  where
    startCoord = findChar 'S' hm ^?! folded
    isExit (Exit _) = True
    isExit _ = False
    heightCondition fromH toH = toH - fromH <= 1
    hm = fromList (unpack <$> toList input)

solve2 :: Foldable t => prev -> t Text -> Maybe Int
solve2 _ input =
    length <$> search hm (Regular startCoord) isExit heightCondition
  where
    startCoord = findChar 'E' hm ^?! folded
    isExit (Regular c) = hm Vec2d.! c == 'a'
    isExit (Exit _) = False
    heightCondition fromH toH = fromH - toH <= 1
    hm = fromList (unpack <$> toList input)

