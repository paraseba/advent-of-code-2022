module Main where

import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Day1 qualified as D1
import Day10 qualified as D10
import Day11 qualified as D11
import Day12 qualified as D12
import Day13 qualified as D13
import Day14 qualified as D14
import Day15 qualified as D15
import Day2 qualified as D2
import Day3 qualified as D3
import Day4 qualified as D4
import Day5 qualified as D5
import Day6 qualified as D6
import Day7 qualified as D7
import Day8 qualified as D8
import Day9 qualified as D9

days :: Seq.Seq (IO ())
days =
    Seq.fromList
        [ D1.main
        , D2.main
        , D3.main
        , D4.main
        , D5.main
        , D6.main
        , D7.main
        , D8.main
        , D9.main
        , D10.main
        , D11.main
        , D12.main
        , D13.main
        , D14.main
        , D15.main
        ]

dayToAction :: Int -> IO ()
dayToAction day =
    fromMaybe (putStrLn "Day not implemented") $ days ^? (ix (day - 1))

parseArgs :: IO [Maybe Int]
parseArgs = getArgs & (mapped.traversed) %~ readMaybe

runDay :: Int -> IO ()
runDay day = do
    putStrLn $ "Running day " <> show day
    dayToAction day

main :: IO ()
main = parseArgs >>= traverseOf_ (traversed._Just) runDay
