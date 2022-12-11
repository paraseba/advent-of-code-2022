{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Day11 where

import Control.Lens hiding ((<|))
import Control.Monad.State (MonadState)
import Data.Foldable (traverse_, toList)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Control.Monad.State.Strict (execState)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Monad (replicateM)
import Data.List (sort)
import Utils

newtype MonkeyId  = MonkeyId Int
  deriving (Num, Eq, Show, Enum)
   

data Monkey =
  Monkey
  { _mItems :: Seq Integer
  , _mOperation :: Integer -> Integer
  , _mDiv :: Integer
  , _mTrueId :: MonkeyId
  , _mFalseId :: MonkeyId
  , _mInspections :: Integer
  }
makeLenses ''Monkey


data Config =
  Config
  { _worryDivFactor :: Integer
  , _modulo :: Integer
  , _numRounds :: Int
  , _monkeyVector :: Vector Monkey
  }
makeLenses ''Config

testMonkeys :: Vector Monkey
testMonkeys = V.fromList 
  [ Monkey (Seq.fromList [79, 98]) (* 19) 23 2 3 0
  , Monkey (Seq.fromList [54, 65, 75, 74]) (+ 6) 19 2 0 0
  , Monkey (Seq.fromList [79, 60, 97]) (\x -> x * x) 13 1 3 0
  , Monkey (Seq.fromList [74]) (+ 3) 17 0 1 0
  ]

inputMonkeys :: Vector Monkey
inputMonkeys = V.fromList 
  [ Monkey (Seq.fromList [89, 95, 92, 64, 87, 68]) (* 11) 2 7 4 0
  , Monkey (Seq.fromList [87, 67]) (+ 1) 13 3 6 0
  , Monkey (Seq.fromList [95, 79, 92, 82, 60]) (+ 6) 3 1 6 0
  , Monkey (Seq.fromList [67, 97, 56]) (\x -> x * x) 17 7 0 0
  , Monkey (Seq.fromList [80, 68, 87, 94, 61, 59, 50, 68]) (* 7) 19 5 2 0
  , Monkey (Seq.fromList [73, 51, 76, 59]) (+ 8) 7 2 1 0
  , Monkey (Seq.fromList [92]) (+ 5) 11 3 0 0
  , Monkey (Seq.fromList [99, 76, 78, 76, 79, 90, 89]) (+ 7) 5 4 5 0
  ]

calcMonkey :: Config -> Monkey -> [(Integer, MonkeyId)]
calcMonkey config monkey =
  monkey ^.. (mItems.traversed.to throw)
  where
    throw worry = (newWorry, throwTo newWorry)
      where newWorry = ((monkey ^. mOperation) worry `div` (config ^. worryDivFactor)) `mod` (config ^. modulo)

    throwTo worry = if worry `mod` (monkey ^. mDiv) == 0
                    then (monkey ^. mTrueId)
                    else (monkey ^. mFalseId)
 
evalMonkey :: MonadState (Vector Monkey) m => Config -> MonkeyId -> m ()
evalMonkey config (MonkeyId idx) = do
    monkey <- use (monkeyL idx)
    traverse_ throw (calcMonkey config monkey)
    (monkeyL idx . mInspections) += fromIntegral (lengthOf (mItems.folded) monkey)
    (monkeyL idx . mItems) .= Seq.empty
  where
    monkeyL i = singular $ ix i
    throw (worry, MonkeyId mid) =
        modifying (monkeyL mid . mItems) (|> worry)

evalRound :: MonadState (Vector Monkey) m => Config -> m ()
evalRound config = do 
    numMonkeys <- uses id length
    traverse_ (evalMonkey config) [0..(MonkeyId numMonkeys - 1)]

run :: Config -> Vector (Integer, Seq Integer)
run config = execState (replicateM rounds (evalRound config)) monkeys <&> extract
  where
    extract monkey = (monkey ^. mInspections, monkey ^. mItems)
    rounds = config ^. numRounds
    monkeys = config ^. monkeyVector

business :: Vector (Integer, Seq Integer) -> Integer
business =  product . take 2 . reverse . sort . map fst . toList

calcModulo :: Vector Monkey -> Integer
calcModulo = productOf (folded.mDiv)

solve1 :: IO ()
solve1 = putStrLn . show $ business $ run config
  where config = Config 3 (calcModulo inputMonkeys) 20 inputMonkeys

solve2 :: prev -> IO ()
solve2 _ = putStrLn . show $ business $ run config
  where config = Config 1 (calcModulo inputMonkeys) 10_000 inputMonkeys

main :: IO ()
main = solve solve1 solve2
