{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Day24 (main) where

import Algorithm.Search (aStar)
import Control.Applicative (many)
import Control.Lens
import Control.Monad.Combinators.NonEmpty (sepEndBy1)
import Control.Monad.ST (ST)
import Data.Bits (Ior (..), testBit)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty.Extra (some1)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Word (Word8)
import Text.Megaparsec (try, (<|>))
import Text.Megaparsec.Char (char, eol)
import Utils
import Vec2d (Col (..), Coord, Row (..), Vec2d (..))
import Vec2d qualified as Vec2d

data Dir = DL | DR | DU | DD

newtype WindDirs = WindDirs Word8
    deriving (Eq, Ord, Show)
    deriving (Semigroup) via (Ior Word8)
    deriving (Monoid) via (Ior Word8)

type ParserOut = NonEmpty (NonEmpty WindDirs)

mapP :: Parser ParserOut
mapP = wallP *> rowsP <* wallP
  where
    wallP :: Parser Col
    wallP = (Col . length) <$> (many (char '#') <* char '.' <* many (char '#') <* eol)
    rowsP :: Parser (NonEmpty (NonEmpty WindDirs))
    rowsP = sepEndBy1 (try (char '#' *> try rowP <* char '#')) eol
    rowP :: Parser (NonEmpty WindDirs)
    rowP = some1 cellP
    cellP :: Parser WindDirs
    cellP =
        singleton DL <$ char '<'
            <|> singleton DR <$ char '>'
            <|> singleton DU <$ char '^'
            <|> singleton DD <$ char 'v'
            <|> mempty <$ char '.'

foldWinds :: (a -> Dir -> a) -> a -> WindDirs -> a
foldWinds f a (WindDirs ws) = ssss
  where
    s = if testBit ws 0 then f a DL else a
    ss = if testBit ws 1 then f s DR else s
    sss = if testBit ws 2 then f ss DU else ss
    ssss = if testBit ws 3 then f sss DD else sss
{-# INLINEABLE foldWinds #-}

singleton :: Dir -> WindDirs
singleton DL = WindDirs 0b00000001
singleton DR = WindDirs 0b00000010
singleton DU = WindDirs 0b00000100
singleton DD = WindDirs 0b00001000

newtype WindMap = WM (Vec2d WindDirs)
    deriving (Show, Ord, Eq)

data SimState = SimState
    { ssCurrent :: Coord
    , ssWindMap :: WindMap
    }
    deriving (Show, Ord, Eq)

initialState :: ParserOut -> SimState
initialState (cells) =
    SimState
        (Row (-1), Col 0)
        (WM $ Vec2d.fromList (toList cells))

updateWind :: WindMap -> WindMap
updateWind wm@(WM v@(Vec2d _ origSize)) = WM (Vec2d (V.create create) origSize)
  where
    create :: forall s. ST s (V.MVector s WindDirs)
    create = ifoldl' (\coord -> foldWinds (go' coord)) (MV.replicate (length v) mempty) v

    gs = gridSize wm

    go' :: Coord -> ST s (V.MVector s WindDirs) -> Dir -> ST s (V.MVector s WindDirs)
    go' coord mv dir = do
        mv' <- mv
        let newCoord = move gs dir coord
        MV.modify mv' (<> singleton dir) (Vec2d.index2d v newCoord)
        pure mv'

move :: Coord -> Dir -> Coord -> Coord
move size DL (r, c) = wrap size (r, c - 1)
move size DR (r, c) = wrap size (r, c + 1)
move size DU (r, c) = wrap size (r - 1, c)
move size DD (r, c) = wrap size (r + 1, c)

wrap :: Coord -> Coord -> Coord
wrap (rmax, cmax) (row, col)
    | row < 0 = (rmax - 1, col)
    | col < 0 = (row, cmax - 1)
    | row == rmax = (0, col)
    | col == cmax = (row, 0)
    | otherwise = (row, col)

gridSize :: WindMap -> Coord
gridSize (WM v) = (Vec2d.numRows' v, Vec2d.numCols' v)

neighbors :: SimState -> [SimState]
neighbors SimState{ssWindMap = origWindMap@(WM origWindMapV), ssCurrent} =
    ssCurrent
        & potentialNeighbors gs
        & filter isFree
        <&> \current -> SimState current wind
  where
    gs@(rmax, cmax) = gridSize origWindMap
    wind = updateWind origWindMap
    isFree coord
        | coord == (-1, 0) = True
        | coord == (rmax, cmax - 1) = True
        | otherwise = origWindMapV Vec2d.! coord == mempty

potentialNeighbors :: Coord -> Coord -> [Coord]
potentialNeighbors (rmax, cmax) current@(row, col) =
    exitNeigh
        <> entryNeigh
        <> [ (r, c)
           | (r, c) <- [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1), (row, col)]
           , r >= 0
           , c >= 0
           , r < rmax
           , c < cmax
           ]
  where
    exitNeigh =
        if
                | row == rmax - 1 && col == cmax - 1 -> [(rmax, col)]
                | row == rmax && col == cmax - 1 -> [(rmax, col)]
                | otherwise -> []
    entryNeigh =
        case current of
            (0, 0) -> [(-1, 0)]
            (-1, 0) -> [(-1, 0)]
            _ -> []

exit :: SimState -> (Row, Col)
exit SimState{..} = let (rmax, cmax) = (gridSize ssWindMap) in (rmax, cmax - 1)

search :: SimState -> Coord -> Maybe (Int, [SimState])
search initial goal = aStar neighbors g h isExit initial
  where
    g _ _ = 1
    h s = manhattan' (ssCurrent s) goal
    isExit s = (ssCurrent s) == goal

manhattan' :: (Row, Col) -> (Row, Col) -> Int
manhattan' (Row r1, Col c1) (Row r2, Col c2) = manhattan (r1, c1) (r2, c2)

main :: IO ()
main = parseAndSolveIO 24 mapP solve1 solve2

solve1 :: ParserOut -> IO (Int, SimState)
solve1 input = do
    let initial = initialState input
    case search initial (exit initial) of
        Just (totalCost, path) -> do
            print $ totalCost - 1
            pure (totalCost - 1, last path)
        Nothing -> error "Cannot find solution"

solve2 :: (Int, SimState) -> prev -> IO ()
solve2 (initialCost, goalState) _ =
    maybe (error "Cannot find solution") print doSearch
  where
    doSearch :: Maybe Int
    doSearch = do
        (costBackHome, path) <- search goalState (-1, 0)
        (costBackGoal, _) <- search (last path) (exit goalState)
        pure $ initialCost + costBackHome + costBackGoal
