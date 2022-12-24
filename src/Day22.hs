{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Day22 where

import Control.Lens hiding (Empty)
import Control.Monad (guard)
import Control.Monad.Combinators.NonEmpty (sepEndBy1)
import Control.Monad.State.Strict (MonadState, execState)
import Data.Foldable (toList, traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Extra (some1)
import Data.Maybe (fromJust)
import Text.Megaparsec ((<|>))
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils
import Vec2d qualified as V

data Instruction = Advance Int | L | R
    deriving (Show, Eq)

type Path = NonEmpty Instruction

pathP :: Parser Path
pathP = some1 instP
  where
    instP =
        Advance <$> num
            <|> L <$ char 'L'
            <|> R <$ char 'R'
    num = do
        n <- decimal
        guard (n > 0)
        pure n

data Cell = Empty | Tile | Wall
    deriving (Show, Eq)

mapLine :: Parser (NonEmpty Cell)
mapLine = some1 cellP
  where
    cellP =
        Empty <$ char ' '
            <|> Tile <$ char '.'
            <|> Wall <$ char '#'

mapP :: Parser (NonEmpty (NonEmpty Cell), Path)
mapP = (,) <$> (sepEndBy1 mapLine eol) <* eol <*> pathP

type Board = V.Vec2d Cell

mkBoard :: NonEmpty (NonEmpty Cell) -> Board
mkBoard cs = V.fromList (toList (complete <$> cs))
  where
    width = maximum1Of (folded . to length) cs
    complete row = NE.take width $ row <> NE.repeat Empty

data Heading = HT | HR | HB | HL
    deriving (Eq, Show)

data SimState = SimState
    { _ssCurrentLoc :: V.Coord
    , _ssCurrentHeading :: Heading
    }
    deriving (Show)

makeLenses ''SimState

initialState :: Board -> SimState
initialState b = SimState (findStart b) HR

findStart :: FoldableWithIndex a f => f Cell -> a
findStart board = fromJust $ fst <$> ifind (\_ c -> isTile c) board

isTile :: Cell -> Bool
isTile Tile = True
isTile _ = False

type Wrapper = (V.Coord, Heading) -> (V.Coord, Heading)

execute :: MonadState SimState m => Wrapper -> Board -> Instruction -> m ()
execute _ _ L =
    use ssCurrentHeading >>= \ch ->
        ssCurrentHeading .= case ch of
            HT -> HL
            HR -> HT
            HB -> HR
            HL -> HB
execute _ _ R =
    use ssCurrentHeading >>= \ch ->
        ssCurrentHeading .= case ch of
            HT -> HR
            HR -> HB
            HB -> HL
            HL -> HT
execute wrap board (Advance steps) = do
    cs <- nextCoords wrap board
    let endedAt = dropToEnd steps (takeWhile (\ch -> board V.! (fst ch) == Tile) cs)
    case firstOf folded endedAt of
        Just (c, h) -> ssCurrentLoc .= c >> ssCurrentHeading .= h
        Nothing -> pure ()

dropToEnd :: (Eq t, Num t) => t -> [a] -> [a]
dropToEnd _ [] = []
dropToEnd _ [a] = [a]
dropToEnd 0 as = as
dropToEnd n (_ : as) = dropToEnd (n - 1) as

nextCoords :: MonadState SimState m => Wrapper -> Board -> m [(V.Coord, Heading)]
nextCoords wrap board = do
    coord <- use ssCurrentLoc
    heading <- use ssCurrentHeading
    pure $ filter (\ch -> board V.! (fst ch) /= Empty) $ iterate advance (coord, heading)
  where
    advance :: (V.Coord, Heading) -> (V.Coord, Heading)
    advance ((row, col), HT) = wrap ((row - 1, col), HT)
    advance ((row, col), HR) = wrap ((row, col + 1), HR)
    advance ((row, col), HB) = wrap ((row + 1, col), HB)
    advance ((row, col), HL) = wrap ((row, col - 1), HL)

wrap1 :: Board -> Wrapper
wrap1 board ((-1, col), h) = ((V.numRows' board - 1, col), h)
wrap1 board ((row, -1), h) = ((row, V.numCols' board - 1), h)
wrap1 board ((row, col), h)
    | row == V.numRows' board = ((0, col), h)
    | col == V.numCols' board = ((row, 0), h)
wrap1 _ (c, h) = (c, h)

password :: (V.Row, V.Col) -> Heading -> Int
password (V.Row r, V.Col c) h = 1000 * (r + 1) + 4 * (c + 1) + facing h
  where
    facing HR = 0
    facing HB = 1
    facing HL = 2
    facing HT = 3

wrap2Ex :: Board -> Wrapper
wrap2Ex _ ((-1, col), _) = ((4, 11 - col), HB) -- 1 going up
wrap2Ex board ((V.Row row, -1), _) = ((V.numRows' board - 1, V.numCols' board - 1 - (V.Col row - 4)), HT) -- 2 going left
wrap2Ex board ((V.Row row, V.Col col), _)
    -- 6 going right
    | col == V.numCols board = ((V.numRows' board - 1 - V.Row row, 3), HL)
    -- 6 going down
    | row == V.numRows board && col >= 12 = ((7 - (V.Row col - 4), V.Col 0), HR)
    -- 6 going up
    | row == 7 && col >= 12 = ((19 - V.Row col, 11), HL)
    -- 5 going down
    | row == V.numRows board && col < 12 = ((V.Row 7, 11 - V.Col col), HT)
    -- 5 going left
    | col == 7 && row >= 8 = ((7, 15 - V.Col row), HT)
    -- 2 going down
    | row == 8 && col < 4 = ((V.numRows' board - 1, 11 - V.Col col), HT)
    -- 2 going up
    | row == 3 && col < 4 = ((0, 11 - V.Col col), HB)
    -- 3 going down
    | row == 8 && col >= 4 && col < 8 = ((11 - (V.Row col - 4), 8), HR)
    -- 3 going up
    | row == 3 && col >= 4 && col < 8 = ((V.Row col - 4, 8), HR)
    -- 4 going right
    | col == 12 && row >= 4 && row < 8 = ((8, 12 + (7 - V.Col row)), HB)
    -- 1 going right
    | col == 12 && row < 4 = ((15 - V.Row row, 15), HL)
    -- 1 going left
    | col == 7 && row < 4 = ((4, 4 + V.Col row), HB)
wrap2Ex _ (c, h) = (c, h)

wrap2 :: Board -> Wrapper
wrap2 _ ((V.Row row, V.Col col), h)
    -- 1 going up into 6
    | h == HT && row == -1 && col < 100 = ((100 + V.Row col, 0), HR)
    -- 1 going left into 4
    | h == HL && row < 50 && col == 49 = ((149 - V.Row row, 0), HR)
    -- 2 going up into 6
    | h == HT && row == -1 && col >= 100 = ((199, V.Col col - 100), HT)
    -- 2 going right into 5
    | h == HR && row < 50 && col == 150 = ((149 - V.Row row, 99), HL)
    -- 2 going down into 3
    | h == HB && row == 50 && col >= 100 = ((V.Row col - 50, 99), HL)
    -- 3 going left into 4
    | h == HL && row < 100 && row >= 50 && col == 49 = ((100, V.Col row - 50), HB)
    -- 3 going right into 2
    | h == HR && row < 100 && row >= 50 && col == 100 = ((49, V.Col row + 50), HT)
    -- 4 going up into 3
    | h == HT && row == 99 && col < 50 = ((V.Row col + 50, 50), HR)
    -- 4 going left into 1
    | h == HL && row >= 100 && row < 150 && col == -1 = ((149 - V.Row row, 50), HR)
    -- 5 going right into 2
    | h == HR && row < 150 && row >= 100 && col == 100 = ((149 - V.Row row, 149), HL)
    -- 5 going down into 6
    | h == HB && row == 150 && col >= 50 = ((V.Row col + 100, 49), HL)
    -- 6 going left into 1
    | h == HL && row >= 150 && col == -1 = ((0, V.Col row - 100), HB)
    -- 6 going down into 2
    | h == HB && row == 200 = ((0, V.Col col + 100), HB)
    -- 6 going right into 5
    | h == HR && row >= 150 && col == 50 = ((149, V.Col row - 100), HT)
wrap2 _ (c, h) = (c, h)

main :: IO ()
main = parseAndSolveIO 22 mapP solve1 solve2

solve1 :: Foldable t => (NonEmpty (NonEmpty Cell), t Instruction) -> IO Board
solve1 (rawMap, path) = do
    let res = execState (traverse_ (execute (wrap1 board) board) path) initial
    print $ password (res ^. ssCurrentLoc) (res ^. ssCurrentHeading)
    pure board
  where
    board = mkBoard rawMap
    initial = initialState board

solve2 :: Foldable t => Board -> (NonEmpty (NonEmpty Cell), t Instruction) -> IO ()
solve2 board (_, path) = do
    let res = execState (traverse_ (execute (wrap2 board) board) path) initial
    print $ password (res ^. ssCurrentLoc) (res ^. ssCurrentHeading)
  where
    initial = initialState board
