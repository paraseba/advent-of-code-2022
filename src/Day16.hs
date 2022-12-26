{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day16 where

import Control.Applicative.Combinators.NonEmpty (sepBy1, sepEndBy1)
import Control.Lens
import Control.Parallel.Strategies (parBuffer, rdeepseq, using)
import Data.Char (isUpper)
import Data.Foldable (foldl', toList)
import Data.Function (on)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Megaparsec (takeP, takeWhileP)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils

type ParserOut = (Text, Int, (NonEmpty Text))

mapP :: Parser (NonEmpty (Text, Int, (NonEmpty Text)))
mapP = sepEndBy1 lineP eol
  where
    lineP =
        (,,)
            <$> (string "Valve " *> valveId)
            <*> (toEqual *> decimal <* takeWhileP Nothing (not . isUpper))
            <*> sepBy1 valveId (string ", ")
      where
        toEqual = takeWhileP Nothing notEq *> char '='
        notEq c = c /= '=' && c /= '\n'
        valveId = takeP (Just "valve") 2

data ValveId = NoValve | ValveId Text
    deriving (Show, Eq, Generic)

instance Hashable ValveId

data Valve = Valve {_vRate :: Int, _vNeighbors :: [ValveId]}
    deriving (Eq, Generic, Show)
makeLenses ''Valve

instance Hashable Valve

type Graph = HM.HashMap ValveId Valve

toGraph :: NonEmpty (Text, Int, (NonEmpty Text)) -> Graph
toGraph = HM.fromList . toList . fmap convert
  where
    convert (name, rate, neighs) = (ValveId name, Valve rate (ValveId <$> toList neighs))

type OpenValves = HS.HashSet Valve

minuteOut :: OpenValves -> Int
minuteOut = sumOf (folded . vRate)

getValve :: Graph -> ValveId -> Valve
getValve = (HM.!)

iCanDo :: Ord a => a -> a -> Maybe a
iCanDo atLeast n = if atLeast < n then Just n else Nothing

usefulValves :: Foldable f => f Valve -> HS.HashSet Valve
usefulValves g = HS.fromList $ toListOf (folded . filtered ((> 0) . (^. vRate))) g

calcFlow :: Graph -> OpenValves -> Int -> ValveId -> Maybe Int
calcFlow graph =
    flow NoValve (-1)
  where
    allValves = usefulValves graph

    flow :: ValveId -> Int -> OpenValves -> Int -> ValveId -> Maybe Int
    flow _ atLeast _ 0 _ = iCanDo atLeast 0
    flow _ atLeast ov 1 _ = iCanDo atLeast (minuteOut ov)
    flow comingFrom moreThan ov availableTime currentPos
        | shouldGiveUp = Nothing
        | otherwise = iCanDo moreThan (foldl' calcNeigh moreThan neighbors)
      where
        calcNeigh currentMax neighbor
            | thisValveRate <= 0 || HS.member thisValve ov = withoutOpening
            | otherwise = max opening withoutOpening
          where
            withoutOpening = fromMaybe currentMax (moveWithoutOpening neighbor currentMax)
            opening = openAndMove neighbor withoutOpening

        thisValve = getValve graph currentPos
        withCurrent = HS.insert thisValve ov
        currentRate = minuteOut ov
        thisValveRate = thisValve ^. vRate
        neighbors = thisValve ^. vNeighbors
        moveWithoutOpening neighbor currentMax =
            if neighbor /= comingFrom
                then -- if we don't come from there try going without opening current
                    (currentRate +) <$> flow currentPos (currentMax - currentRate) ov (availableTime - 1) neighbor -- go to the neighbor without opening
                else Nothing
        openAndMove neighbor currentMax = case flow currentPos neighRequired withCurrent (availableTime - 2) (neighbor) of
            Just newMax -> newMax + currentRate + currentRate + thisValveRate
            Nothing -> currentMax
          where
            neighRequired =
                currentMax - currentRate - currentRate - thisValveRate

        shouldGiveUp = achievable <= moreThan
          where
            closedValves = toList (HS.difference allValves ov)
            closedFlows = sortBy (flip (compare `on` (^. vRate))) closedValves
            closedRates = closedFlows ^.. (folded . vRate)
            achievable = currentRate * availableTime + maxFutureFlow
            maxFutureFlow = sum $ zipWith (*) openTimes closedRates
            openTimes = [availableTime - 1, availableTime - 3 .. 0]

partitions :: [a] -> [([a], [a])]
partitions =
    foldr
        (\x -> concatMap (\(lxs, rxs) -> [(x : lxs, rxs), (lxs, x : rxs)]))
        [([], [])]

main :: IO ()
main = parseAndSolve 16 mapP solve1 solve2

solve1 :: NonEmpty (Text, Int, NonEmpty Text) -> Maybe Int
solve1 input = calcFlow graph HS.empty 30 (ValveId "AA")
  where
    graph = toGraph input

solve2 :: prev -> NonEmpty (Text, Int, NonEmpty Text) -> Maybe Int
solve2 _ input =
    maximumOf (folded . _Just) ((process2 <$> splitWork) `using` parBuffer 15 rdeepseq)
  where
    graph = toGraph input
    nonZeroValves = toListOf (folded . filteredBy (_2 . filtered (> 0)) . _1 . to ValveId) input
    zeroValvesPart = partitions nonZeroValves
    splitWork = take (length zeroValvesPart `div` 2) zeroValvesPart
    zeroGraph :: Graph -> [ValveId] -> Graph
    zeroGraph g = foldr (\vid g' -> HM.adjust (\v -> v & vRate .~ 0) vid g') g
    process zeroValves = calcFlow (zeroGraph graph zeroValves) HS.empty 26 (ValveId "AA")
    process2 (zv1, zv2) = (+) <$> (process zv1) <*> process zv2

