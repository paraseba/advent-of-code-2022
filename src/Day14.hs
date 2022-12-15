{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day14 where

import Control.Applicative ((<|>))
import Control.Applicative.Combinators.NonEmpty (sepBy1, sepEndBy1)
import Control.Lens hiding ((<|))
import Data.Foldable (toList)
import Data.HashMap.Strict qualified as M
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils

coordP :: Parser (Int, Int)
coordP = (,) <$> decimal <* char ',' <*> decimal

type Path = NonEmpty (Int, Int)

pathP :: Parser Path
pathP = sepBy1 coordP (string " -> ")

caveP :: Parser (NonEmpty Path)
caveP = sepEndBy1 pathP eol

data Solid = Sand | Rock
    deriving (Show)

type Coord = (Int, Int)

data SimState = SimState
    { ssMap :: M.HashMap Coord Solid
    , ssCurrent :: Coord
    , ssResting :: Int
    }
    deriving (Show)

sandSource :: (Int, Int)
sandSource = (500, 0)

initState :: NonEmpty Path -> SimState
initState paths = SimState (foldr insert M.empty paths) sandSource 0
  where
    insert :: Path -> M.HashMap Coord Solid -> M.HashMap Coord Solid
    insert path res =
        foldr insertSegment res (zipWith (,) (toList path) (NE.tail path))

    insertSegment ((fromx, fromy), (tox, toy)) m =
        m `M.union` M.fromList [((x, y), Rock) | x <- [min fromx tox .. max fromx tox], y <- [min fromy toy .. max fromy toy]]

findBottom :: M.HashMap Coord Solid -> Int
findBottom m = fromJust $ maximumOf (folded . _2) (M.keys m)

simulate1 :: Int -> SimState -> Maybe SimState
simulate1 bottom state@SimState{..} =
    case move ssMap ssCurrent of
        Just newPos
            | snd newPos > bottom -> Nothing
            | otherwise -> Just $ state{ssCurrent = newPos}
        Nothing ->
            Just $
                state
                    { ssCurrent = sandSource
                    , ssResting = ssResting + 1
                    , ssMap = M.insert ssCurrent Sand ssMap
                    }

simulate2 :: Int -> SimState -> Maybe SimState
simulate2 bottom state@SimState{..}
    | M.member sandSource ssMap = Nothing
    | snd ssCurrent == bottom + 1 = newGrain
    | otherwise = case move ssMap ssCurrent of
        Just newPos -> Just state{ssCurrent = newPos}
        Nothing -> newGrain
  where
    newGrain =
        Just
            state
                { ssCurrent = sandSource
                , ssResting = ssResting + 1
                , ssMap = M.insert ssCurrent Sand ssMap
                }

move :: M.HashMap Coord Solid -> Coord -> Maybe Coord
move m (x, y) =
    empty (x, y + 1)
        <|> empty (x - 1, y + 1)
        <|> empty (x + 1, y + 1)
  where
    empty :: Coord -> Maybe Coord
    empty coord = if M.member coord m then Nothing else Just coord

main :: IO ()
main = parseAndSolve 14 caveP solve1 solve2

run :: (a -> Maybe a) -> a -> a
run f a = maybe a (run f) $ f a

solve1 :: NonEmpty Path -> Int
solve1 cave = ssResting $ run (simulate1 b) s
  where
    s = initState cave
    b = findBottom (ssMap s)

solve2 :: prev -> NonEmpty Path -> Int
solve2 _ cave = ssResting $ run (simulate2 b) s
  where
    s = initState cave
    b = findBottom (ssMap s)
