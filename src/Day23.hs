{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Day23 where

import Control.Applicative (asum)
import Control.Lens hiding (Empty)
import Control.Monad.Combinators.NonEmpty (sepEndBy1)
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty.Extra (some1)
import Data.Maybe (fromJust)
import Text.Megaparsec ((<|>))
import Text.Megaparsec.Char (char, eol)
import Utils
import Vec2d (Coord)
import Vec2d qualified as V
import Prelude hiding (round)

data Cell = Tile | Elve
    deriving (Eq, Show)

mapP :: Parser (NonEmpty (NonEmpty Cell))
mapP = sepEndBy1 rowP eol
  where
    rowP :: Parser (NonEmpty Cell)
    rowP = some1 cellP
    cellP = Tile <$ char '.' <|> Elve <$ char '#'

data Dir = North | South | West | East
    deriving (Eq, Show, Enum)

roundDirections :: [Dir]
roundDirections = cycle [North .. East]

data SimState = SimState
    { ssElves :: HashSet Coord
    , ssRoundDir :: [Dir]
    }

initialState :: Foldable t => [t Cell] -> SimState
initialState cells =
    SimState (HS.fromList $ fst <$> coords) roundDirections
  where
    isElve Elve = True
    isElve Tile = False
    v = V.fromList cells
    coords = itoListOf (ifolded . filtered isElve) v

round :: SimState -> SimState
round state@SimState{..} =
    let proposals = validProposals (propose state)
        go (fromCoord, toCoord) s = s & HS.delete fromCoord & HS.insert toCoord
        newPositions = foldr go ssElves proposals
     in SimState newPositions (drop 1 ssRoundDir)

validProposals :: HashMap Coord [Coord] -> [(Coord, Coord)]
validProposals m =
    HM.toList valid <&> \case
        (toCoord, [fromCoord]) -> (fromCoord, toCoord)
        _ -> error "Internal error"
  where
    valid = HM.filter (\proponents -> length proponents == 1) m

propose :: SimState -> HashMap Coord [Coord]
propose SimState{..} =
    foldr go HM.empty ssElves
  where
    go coord m
        | hasNeighbors coord ssElves = case choseProposal coord of
            Just toCoord -> HM.insertWith (<>) toCoord [coord] m
            Nothing -> m
        | otherwise = m

    choseProposal coord = asum (checkDirection <$> checks coord ssRoundDir)

    checkDirection :: (Coord, [Coord]) -> Maybe Coord
    checkDirection (c, coords) =
        if any (flip HS.member ssElves) coords
            then Nothing
            else Just c

hasNeighbors :: Coord -> HashSet Coord -> Bool
hasNeighbors c m = any (flip HS.member m) (neighbors c)

neighbors :: Coord -> [Coord]
neighbors (r, c) =
    [ (r - 1, c - 1)
    , (r - 1, c)
    , (r - 1, c + 1)
    , (r, c - 1)
    , (r, c + 1)
    , (r + 1, c - 1)
    , (r + 1, c)
    , (r + 1, c + 1)
    ]

checks :: Coord -> [Dir] -> [(Coord, [Coord])]
checks coord@(r, c) dirs =
    check <$> take 4 dirs
  where
    neigh = neighbors coord
    check North = ((r - 1, c), filter (\(r', _) -> r' < r) neigh)
    check South = ((r + 1, c), filter (\(r', _) -> r' > r) neigh)
    check East = ((r, c + 1), filter (\(_, c') -> c' > c) neigh)
    check West = ((r, c - 1), filter (\(_, c') -> c' < c) neigh)

minRect :: HashSet Coord -> (Coord, Coord)
minRect coords = ((xmin, ymin), (xmax, ymax))
  where
    xmin = fromJust $ minimumOf (folded . _1) coords
    ymin = fromJust $ minimumOf (folded . _2) coords
    xmax = fromJust $ maximumOf (folded . _1) coords
    ymax = fromJust $ maximumOf (folded . _2) coords

emptyTiles :: HashSet Coord -> Int
emptyTiles coords =
    let ((xmin, ymin), (xmax, ymax)) = minRect coords
     in length [(x, y) | x <- [xmin .. xmax], y <- [ymin .. ymax], not $ HS.member (x, y) coords]

main :: IO ()
main = parseAndSolveIO 23 mapP solve1 solve2

solve1 :: (Foldable t1, Foldable t2) => t2 (t1 Cell) -> IO SimState
solve1 input = do
    let finalCoords = ssElves $ iterate round initial !! 10
        res = emptyTiles finalCoords
    print res
    pure initial
  where
    initial = initialState (toList input)

solve2 :: SimState -> p -> IO ()
solve2 initial _ = do
    let finalCoords = ssElves <$> iterate round initial
        fcShifted = drop 1 finalCoords
        comp = takeWhile (uncurry (/=)) (zip finalCoords fcShifted)
    print $ length comp + 1
