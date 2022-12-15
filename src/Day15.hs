{-# LANGUAGE OverloadedStrings #-}

module Day15 where

import Control.Applicative.Combinators.NonEmpty (sepEndBy1)
import Control.Lens
import Data.Foldable (toList)
import Data.IntegerInterval qualified as II
import Data.Interval (Interval, (<=..<=))
import Data.Interval qualified as I
import Data.IntervalSet qualified as IS
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Text.Megaparsec (takeWhileP)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Utils

lineP :: Parser ((Int, Int), (Int, Int))
lineP =
    (,)
        <$> ((,) <$> (toEqual *> num) <*> (toEqual *> num))
        <*> ((,) <$> (toEqual *> num) <*> (toEqual *> num))
  where
    toEqual = takeWhileP Nothing notEq *> char '='
    notEq c = c /= '=' && c /= '\n'
    num = signed (pure ()) decimal

logP :: Parser (NonEmpty ((Int, Int), (Int, Int)))
logP = sepEndBy1 lineP eol

data Sensor = Sensor
    { sensorPos :: (Int, Int)
    , sensorBeaconDistance :: Int
    }
    deriving (Show)

toSensor :: ((Int, Int), (Int, Int)) -> Sensor
toSensor (sp, bp) = Sensor sp (manhattan sp bp)

type Ival = Interval Int
type IvalSet = IS.IntervalSet Int

noBeacons :: Int -> Sensor -> IvalSet
noBeacons y (Sensor (sx, sy) distance)
    | dy > distance = IS.empty
    | otherwise = IS.singleton $ low <=..<= high
  where
    dy = abs (y - sy)
    low = I.Finite (sx - (distance - dy))
    high = I.Finite (sx + (distance - dy))

count :: IvalSet -> NonEmpty ((Int, Int), (Int, Int)) -> Int -> Int
count ranges hardware y =
    sumOf (folded . to I.width . to succ) (IS.toList ranges) - others
  where
    sensors = Set.fromList $ NE.filter ((== y) . snd) (fst <$> hardware)
    beacons = Set.fromList $ NE.filter ((== y) . snd) (snd <$> hardware)
    others = length sensors + length beacons

candidateIntervals :: Int -> NonEmpty ((Int, Int), (Int, Int)) -> IvalSet
candidateIntervals line = IS.unions . toList . fmap (noBeacons line . toSensor)

main :: IO ()
main = parseAndSolve 15 logP solve1 solve2

solve1 :: NonEmpty ((Int, Int), (Int, Int)) -> Int
solve1 sensorLog =
    count candidates sensorLog line
  where
    candidates = candidateIntervals line sensorLog
    -- line = 10
    line = 2_000_000

solve2 :: prev -> NonEmpty ((Int, Int), (Int, Int)) -> Maybe Integer
solve2 _ sensorLog =
    let intervals = [0 .. limit] <&> (\row -> IS.difference whole (candidateIntervals row sensorLog))
        solution = ifind (const $ not . IS.null) intervals
        y = fromIntegral . fst <$> solution
        x = solution >>= getX . snd
     in tuningFreq <$> x <*> y
  where
    -- limit = 20
    limit = 4_000_000
    whole = IS.singleton $ 0 <=..<= I.Finite limit
    getX = II.simplestIntegerWithin . II.fromInterval . I.mapMonotonic fromIntegral . head . IS.toList
    tuningFreq x y = x * 4_000_000 + y
