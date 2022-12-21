{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day19 where

import Control.Applicative (Alternative)
import Control.Lens
import Control.Monad (guard)
import Control.Monad.Combinators.NonEmpty (sepEndBy1)
import Data.Foldable (foldl', toList)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Text.Megaparsec.Char (eol, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils

data Blueprint = Blueprint
    { blueprintId :: Int
    , oreRCost :: Int
    , clayRCost :: Int
    , obsidianROreCost :: Int
    , obsidianRClayCost :: Int
    , geodeROreCost :: Int
    , geodeRObsidianCost :: Int
    }
    deriving (Show)

blueprintsP :: Parser (NonEmpty Blueprint)
blueprintsP = sepEndBy1 p eol
  where
    p =
        ( \idd oreore clayore obsidianore obsidianclay geodeore geodeobsidian ->
            Blueprint
                { blueprintId = idd
                , oreRCost = oreore
                , clayRCost = clayore
                , obsidianROreCost = obsidianore
                , obsidianRClayCost = obsidianclay
                , geodeROreCost = geodeore
                , geodeRObsidianCost = geodeobsidian
                }
        )
            <$> (string "Blueprint " *> decimal)
            <*> (string ": Each ore robot costs " *> decimal <* string " ore. ")
            <*> (string "Each clay robot costs " *> decimal <* string " ore. ")
            <*> (string "Each obsidian robot costs " *> decimal <* string " ore and ")
            <*> (decimal <* " clay. ")
            <*> (string "Each geode robot costs " *> decimal <* string " ore and ")
            <*> (decimal <* " obsidian.")

data Stock = Stock
    { stOre :: Int
    , stOreRobots :: Int
    , stClay :: Int
    , stClayRobots :: Int
    , stObsidian :: Int
    , stObsidianRobots :: Int
    , stGeode :: Int
    , stGeodeRobots :: Int
    }
    deriving (Show, Eq)

instance Semigroup Stock where
    s1 <> s2 =
        Stock
            { stOre = stOre s1 + stOre s2
            , stOreRobots = stOreRobots s1 + stOreRobots s2
            , stClay = stClay s1 + stClay s2
            , stClayRobots = stClayRobots s1 + stClayRobots s2
            , stObsidian = stObsidian s1 + stObsidian s2
            , stObsidianRobots = stObsidianRobots s1 + stObsidianRobots s2
            , stGeode = stGeode s1 + stGeode s2
            , stGeodeRobots = stGeodeRobots s1 + stGeodeRobots s2
            }

initialStock :: Stock
initialStock =
    Stock
        { stOre = 0
        , stOreRobots = 1
        , stClay = 0
        , stClayRobots = 0
        , stObsidian = 0
        , stObsidianRobots = 0
        , stGeode = 0
        , stGeodeRobots = 0
        }

search :: Int -> Blueprint -> NonEmpty Stock
search 0 _ = NE.singleton initialStock
search n b = optimize . optiFilter b $ do
    prev <- search (n - 1) b
    built <- prev NE.:| build b prev
    pure $ wait prev <> built

optimize :: NonEmpty Stock -> NonEmpty Stock
optimize ss =
    foldl' comp (NE.singleton start) more
  where
    (start NE.:| more) = NE.sortBy (flip compare `on` nums) ss
    nums Stock{..} = (stOre, stOreRobots, stClay, stClayRobots, stObsidian, stObsidianRobots, stGeode, stGeodeRobots)
    comp res@(prev NE.:| rest) s
        | superior prev s = res
        | superior s prev = s NE.:| rest
        | otherwise = if s /= prev then s NE.:| toList res else res

optiFilter :: Blueprint -> NonEmpty Stock -> NonEmpty Stock
optiFilter b ss =
    let maxOreCost = maximum [oreRCost b, clayRCost b, obsidianROreCost b, geodeROreCost b]
        maxClayCost = obsidianRClayCost b
        maxObsidianCost = geodeRObsidianCost b
        good s =
            stOreRobots s <= maxOreCost
                && stClayRobots s <= maxClayCost
                && stObsidianRobots s <= maxObsidianCost
     in NE.fromList $ filter good (toList ss)

superior :: Stock -> Stock -> Bool
superior s candidate =
    all (uncurry (>=)) (zip (nums s) (nums candidate))
  where
    nums Stock{..} = [stOre, stOreRobots, stClay, stClayRobots, stObsidian, stObsidianRobots, stGeode, stGeodeRobots]

build :: Blueprint -> Stock -> [Stock]
build b s =
    geodeRobots b s
        ++ obsidianRobots b s
        ++ clayRobots b s
        ++ oreRobots b s

wait :: Stock -> Stock
wait Stock{..} =
    Stock
        { stOre = stOreRobots
        , stClay = stClayRobots
        , stObsidian = stObsidianRobots
        , stGeode = stGeodeRobots
        , stOreRobots = 0
        , stClayRobots = 0
        , stObsidianRobots = 0
        , stGeodeRobots = 0
        }

oreRobots :: (Monad m, Alternative m) => Blueprint -> Stock -> m Stock
oreRobots Blueprint{..} s@Stock{..} = do
    guard (stOre >= oreRCost)
    pure
        s
            { stOreRobots = stOreRobots + 1
            , stOre = stOre - oreRCost
            }

clayRobots :: (Monad m, Alternative m) => Blueprint -> Stock -> m Stock
clayRobots Blueprint{..} s@Stock{..} = do
    guard (stOre >= clayRCost)
    pure
        s
            { stClayRobots = stClayRobots + 1
            , stOre = stOre - clayRCost
            }

obsidianRobots :: (Monad m, Alternative m) => Blueprint -> Stock -> m Stock
obsidianRobots Blueprint{..} s@Stock{..} = do
    guard (stOre >= obsidianROreCost && stClay >= obsidianRClayCost)
    pure
        s
            { stObsidianRobots = stObsidianRobots + 1
            , stOre = stOre - obsidianROreCost
            , stClay = stClay - obsidianRClayCost
            }

geodeRobots :: (Monad m, Alternative m) => Blueprint -> Stock -> m Stock
geodeRobots Blueprint{..} s@Stock{..} = do
    guard (stOre >= geodeROreCost && stObsidian >= geodeRObsidianCost)
    pure
        s
            { stGeodeRobots = stGeodeRobots + 1
            , stOre = stOre - geodeROreCost
            , stObsidian = stObsidian - geodeRObsidianCost
            }

main :: IO ()
main = parseAndSolveIO 19 blueprintsP solve1 solve2

solve1 :: NonEmpty Blueprint -> IO ()
solve1 bs = do
    print $ sumOf (folded . to qty) bs
  where
    qty b = blueprintId b * (maximum1Of (folded . to stGeode) $ search 24 b)

solve2 :: prev -> NonEmpty Blueprint -> IO ()
solve2 _ bs = do
    print $ product $ maxGeode <$> (NE.take 3 bs)
  where
    maxGeode b = maximum1Of (folded . to stGeode) $ search 32 b
