{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (void)
import Data.Char (digitToInt)
import Data.Foldable (foldl', toList)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty, some1)
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (digitChar, newline)
import Utils
import Vec2d

end :: Parser ()
end = void newline <|> eof

mapP :: Parser (NonEmpty (NonEmpty Int))
mapP = some1 ((some1 (digitToInt <$> digitChar)) <* end)

data Dir = DTop | DRight | DBottom | DLeft
    deriving (Eq, Ord, Show)

type HeightMap = Vec2d Int
type VisibilityMap = Vec2d (Set Dir)

hmP :: Parser HeightMap
hmP = toVec <$> mapP
  where
    toVec = fromList . fmap toList . toList

fullyVisible :: Set a
fullyVisible = Set.empty

initialVisMap :: Int -> VisibilityMap
initialVisMap n = fromList $ replicate n (replicate n fullyVisible)

travel :: Int -> Dir -> [[Coord]]
travel amount dir = case dir of
    DTop -> [[(row, col) | row <- [0 .. (nrow - 1)]] | col <- [0 .. (ncol - 1)]]
    DBottom -> [[(row, col) | row <- [(nrow - 1), (nrow - 2) .. 0]] | col <- [0 .. (ncol - 1)]]
    DLeft -> [[(row, col) | col <- [0 .. (ncol - 1)]] | row <- [0 .. (nrow - 1)]]
    DRight -> [[(row, col) | col <- [(ncol - 1), (ncol - 2) .. 0]] | row <- [0 .. (nrow - 1)]]
  where
    nrow = fromIntegral amount
    ncol = fromIntegral amount

inspectLine :: HeightMap -> [Coord] -> [Coord]
inspectLine hm coords =
    snd $ foldl' check (-1, []) coords
  where
    check (top, invisibles) coord =
        let h = hm ! coord
         in if h > top
                then (h, invisibles)
                else (top, coord : invisibles)

inspectDir :: HeightMap -> [[Coord]] -> [Coord]
inspectDir hm coords = concatMap (inspectLine hm) coords

markInvisible :: Dir -> [Coord] -> VisibilityMap -> VisibilityMap
markInvisible dir coords vm = foldr modify vm coords
  where
    modify coord v = v // [(coord, Set.insert dir (v ! coord))]

solve1 :: HeightMap -> Int
solve1 hm =
    initialVisMap n
        & markInvisible DLeft (inspectDir hm coordsL)
        & markInvisible DRight (inspectDir hm coordsR)
        & markInvisible DTop (inspectDir hm coordsT)
        & markInvisible DBottom (inspectDir hm coordsB)
        & lengthOf (folded . filtered (\s -> length s < 4))
  where
    n = numRows hm
    coordsL = travel n DLeft
    coordsR = travel n DRight
    coordsT = travel n DTop
    coordsB = travel n DBottom

visibleCoords :: Int -> Coord -> [[Coord]]
visibleCoords n (row, col) =
    [ [(row, col') | col' <- [col - 1, col - 2 .. 0]]
    , [(row, col') | col' <- [col + 1 .. fromIntegral n - 1]]
    , [(row', col) | row' <- [row - 1, row - 2 .. 0]]
    , [(row', col) | row' <- [row + 1 .. fromIntegral n - 1]]
    ]

dirVisibility :: HeightMap -> Coord -> [Coord] -> Int
dirVisibility vm current others =
    case (vm !) <$> others of
        [] -> 0
        heights ->
            let lower = lengthOf (takingWhile (< h) traversed) heights
             in if length others == lower then lower else lower + 1
  where
    h = vm ! current

solve2 :: prev -> HeightMap -> Maybe (Coord, Int)
solve2 _ hm = maximumByOf folded (compare `on` snd) $ (\c -> (c, visibility c)) <$> allCoords
  where
    n = numRows hm
    allCoords = [(row, col) | row <- [0 .. fromIntegral n - 1]
                            , col <- [0 .. fromIntegral n - 1]
                ]
    visibility c =
        dirVisibility hm c (visibleCoords n c !! 0)
            * dirVisibility hm c (visibleCoords n c !! 1)
            * dirVisibility hm c (visibleCoords n c !! 2)
            * dirVisibility hm c (visibleCoords n c !! 3)

main :: IO ()
main = parseAndSolve 8 hmP solve1 solve2
