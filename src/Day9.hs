{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Day9 where

import Control.Applicative (Alternative (empty), (<|>))
import Control.Lens
import Control.Monad (replicateM_, void)
import Control.Monad.RWS.Strict (runRWS)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Writer.Class (tell)
import Data.Foldable (foldl', traverse_)
import Data.List.NonEmpty (NonEmpty (..), some1)
import Data.Semigroup (Endo (..))
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V
import Text.Megaparsec (eof, oneOf)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils

data Move = MUp | MRight | MDown | MLeft
    deriving (Eq, Show)

charToMove :: Char -> Maybe Move
charToMove 'U' = Just MUp
charToMove 'R' = Just MRight
charToMove 'D' = Just MDown
charToMove 'L' = Just MLeft
charToMove _ = Nothing

end :: Parser ()
end = (void eol) <|> eof

path :: Parser (NonEmpty (Move, Int))
path = some1 (movement <* end)
  where
    dir :: Parser (Move)
    dir = oneOf "URDL" >>= maybe empty pure . charToMove

    movement :: Parser (Move, Int)
    movement = (,) <$> (dir <* char ' ') <*> decimal

data Pos = Pos Int Int
    deriving (Eq, Ord, Show)

initialState :: Int -> Vector Pos
initialState numKnots = V.replicate numKnots (Pos 0 0)

moveHead :: (MonadWriter (Endo [Pos]) m, MonadState (Vector Pos) m) => Move -> m ()
moveHead move = do
    knots <- use id
    let oldHP = V.head knots
        newHP = shift oldHP move
        newKnots = follow newHP knots
        t = V.last newKnots
    id .= newKnots
    tell $ Endo ([t] <>)

follow :: Pos -> Vector Pos -> Vector Pos
follow newHead knots =
    let rest = V.tail knots
        go (prev, res) pos = let newPos = followSingle prev pos in (newPos, newPos : res)
     in foldl' go (newHead, []) rest ^. (_2 . reversed . to (newHead :) . to V.fromList)

followSingle :: Pos -> Pos -> Pos
followSingle (Pos hx hy) t@(Pos tx ty) =
    if
            | abs dx <= 1 && abs dy <= 1 -> t
            | dx == 0 -> Pos tx (ty + sign dy)
            | dy == 0 -> Pos (tx + sign dx) ty
            | otherwise -> Pos (tx + sign dx) (ty + sign dy)
  where
    dx = (hx - tx)
    dy = (hy - ty)
    sign :: Int -> Int
    sign n
        | n > 0 = 1
        | otherwise = -1

shift :: Pos -> Move -> Pos
shift (Pos x y) MUp = Pos x (y + 1)
shift (Pos x y) MRight = Pos (x + 1) y
shift (Pos x y) MDown = Pos x (y - 1)
shift (Pos x y) MLeft = Pos (x - 1) y

applyMoves :: Foldable t => Int -> t (Move, Int) -> [Pos]
applyMoves knots moves =
    let (_, _, ps) = runRWS action () (initialState knots)
     in appEndo ps []
  where
    action = traverse_ applyMove moves
    applyMove (move, n) = replicateM_ n (moveHead move)

main :: IO ()
main = parseAndSolve 9 path (calculateBoth solve1 solve2)

solve1 :: NonEmpty (Move, Int) -> Int
solve1 = lengthOf folded . Set.fromList . applyMoves 2

solve2 :: NonEmpty (Move, Int) -> Int
solve2 = lengthOf folded . Set.fromList . applyMoves 10
