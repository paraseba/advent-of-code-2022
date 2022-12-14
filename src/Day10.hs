{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import Control.Applicative ((<|>))
import Control.Lens hiding ((<|))
import Control.Monad (void)
import Control.Monad.State (MonadState)
import Control.Monad.State.Strict (execState)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..), some1, (<|))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (eol, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Utils
import Prelude hiding (log)

data Instruction
    = Addx Integer
    | Noop
    deriving (Eq, Show)

insP :: Parser Instruction
insP = noop <|> addx
  where
    addx = Addx <$> (string "addx " *> signed (pure ()) decimal)
    noop = string "noop" $> Noop

end :: Parser ()
end = void eol <|> eof

programP :: Parser (NonEmpty Instruction)
programP = some1 (insP <* end)

type Clock = Int
type RegisterValue = Integer

initialState :: NonEmpty (Clock, RegisterValue)
initialState = NE.singleton (1, 1)

process :: MonadState (NonEmpty (Clock, RegisterValue)) m => Instruction -> m ()
process Noop = do
    (clock, reg) <- use (id . to NE.head)
    modifying id ((clock + 1, reg) <|)
process (Addx n) = do
    (clock, reg) <- use (id . to NE.head)
    modifying id ((clock + 2, reg + n) <|)

main :: IO ()
main = parseAndSolveIO 10 programP (putStrLn . show . solve1) solve2

getRegValue :: (Ord b, Field1 s s b b, Foldable f, Field2 s s a a) => f s -> b -> a
getRegValue log clock =
    (fromJust $ lastOf (takingWhile ((<= clock) . (^. _1)) folded) log) ^. _2

solve1 :: Foldable t => t Instruction -> RegisterValue
solve1 program =
    [20, 60, 100, 140, 180, 220] <&> (\clock -> fromIntegral clock * (getRegValue log clock)) & sumOf folded
  where
    log = NE.reverse $ execState (traverse_ process program) initialState

pixels :: [[(Clock, Clock)]]
pixels = [[(row, col) | col <- [1 .. 40]] | row <- [0 .. 5]]

solve2 :: Foldable t => prev -> t Instruction -> IO ()
solve2 _ program = do
    traverse_ putStrLn res
  where
    log = NE.reverse $ execState (traverse_ process program) initialState
    res = pixels <&> fmap (\(row, col) -> if abs (getRegValue log (40 * row + col) - fromIntegral (col - 1)) <= 1 then '#' else ' ')
