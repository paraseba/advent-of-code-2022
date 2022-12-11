{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (void)
import Data.Char (digitToInt)
import Data.List.NonEmpty (NonEmpty, some1)
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (newline, digitChar)
import Utils
import Data.Vector qualified as V
import Data.Foldable (toList)
import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function (on)


end :: Parser ()
end = void newline <|> eof

mapP :: Parser (NonEmpty (NonEmpty Int))
mapP = some1 ((some1 (digitToInt <$> digitChar)) <* end)

data Dir = DTop | DRight | DBottom | DLeft
    deriving (Eq, Ord, Show)

type HeightMap = V.Vector (V.Vector Int)
type VisibilityMap = V.Vector (V.Vector (Set Dir))

mapAt :: HeightMap -> (Int, Int) -> Int
mapAt m (y, x) = m V.! y V.! x

hmP :: Parser HeightMap
hmP = toV <$> fmap toV <$> mapP
  where
    toV = V.fromList . toList


fullyVisible :: Set a
fullyVisible = Set.empty

initialVisMap :: Int -> VisibilityMap
initialVisMap n = V.replicate n (V.replicate n fullyVisible)

type Coords = (Int, Int)
travel :: Int -> Dir -> [[Coords]]
travel n DTop = [[(row, col) | row <- [0..(n-1)]] | col <- [0..(n - 1)] ]
travel n DBottom = [[(row, col) | row <- [(n-1),(n-2)..0]] | col <- [0..(n - 1)] ] -- [(row, col) | col <- [0..(n - 1)], row <- [(n - 1), (n-2)..0] ]
travel n DLeft = [[(row, col) | col <- [0..(n-1)]] | row <- [0..(n - 1)] ]
travel n DRight = [[(row, col) | col <- [(n-1),(n-2)..0]] | row <- [0..(n - 1)] ]


inspectLine :: HeightMap -> [Coords] -> [Coords]
inspectLine hm coords =
    snd $ foldl' check (-1, []) coords
    where
        check (top, invisibles) coord = 
            let h = mapAt hm coord
            in if h > top
                then (h, invisibles)
                else (top, coord : invisibles)

inspectDir :: HeightMap -> [[Coords]] -> [Coords]
inspectDir hm coords = concatMap (inspectLine hm) coords

markInvisible :: Dir -> [Coords] -> VisibilityMap -> VisibilityMap
markInvisible dir coords vm = foldr modify vm coords
    where
        modify (row, col) v =
            let rowv = v V.! row
                current = rowv V.! col
                rowv' = rowv V.// [(col, Set.insert dir current)]
            in v V.// [(row, rowv')]


solve1 :: HeightMap -> Int
solve1 hm =
    initialVisMap n
    & markInvisible DLeft (inspectDir hm coordsL)
    & markInvisible DRight (inspectDir hm coordsR)
    & markInvisible DTop (inspectDir hm coordsT)
    & markInvisible DBottom (inspectDir hm coordsB)
    & lengthOf (folded.folded.filtered (\s -> length s < 4))
  where
    n = length hm
    coordsL = travel n DLeft
    coordsR = travel n DRight
    coordsT = travel n DTop
    coordsB = travel n DBottom

visibleCoords :: Int -> Coords -> [[Coords]]
visibleCoords n (row, col) =
    [ [(row, col')| col' <- [col - 1,col-2..0]]
    , [(row, col')| col' <- [col + 1 .. n - 1]]
    , [(row', col)| row' <- [row - 1,row-2..0]]
    , [(row', col)| row' <- [row + 1 .. n - 1]]
    ] -- & filter (not . null)

dirVisibility :: HeightMap -> Coords -> [Coords] -> Int
dirVisibility vm current others =
    case mapAt vm <$> others of
      [] -> 0
      heights ->  let lower = lengthOf (takingWhile (< h) traversed) heights
                  in if length others == lower then lower else lower + 1
  where h = mapAt vm current

solve2 :: prev -> HeightMap -> Maybe (Coords, Int)
solve2 _ hm = maximumByOf folded (compare `on` snd) $ (\c -> (c, visibility c)) <$> allCoords
  where
    n = length hm
    allCoords = [(row, col)| row <- [0..n-1], col <- [0..n-1] ]
    visibility c = 
        dirVisibility hm c (visibleCoords n c !! 0)
        * dirVisibility hm c (visibleCoords n c !! 1)
        * dirVisibility hm c (visibleCoords n c !! 2)
        * dirVisibility hm c (visibleCoords n c !! 3)

main :: IO ()
main = parseAndSolve 8 hmP solve1 solve2

