{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import Utils
import Text.Megaparsec.Char (string, eol)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Functor (void)
import Text.Megaparsec (eof)
import Control.Applicative ((<|>))
import Data.List.NonEmpty (NonEmpty, some1)
import Control.Monad.State (State)
import Control.Lens
import qualified Data.Sequence as Seq
import Control.Monad.State (execState)
import Control.Monad (replicateM_)
import Data.Foldable (toList)

data Action = Action
    { actionQty :: Int
    , actionFrom :: Int
    , actionTo :: Int
    }
    deriving (Show)

action :: Parser Action
action =
    Action
    <$> (string "move " *> decimal <* " from ")
    <*> (decimal <* string " to ")
    <*> (decimal <* (void eol <|> eof))

actions :: Parser (NonEmpty Action)
actions = some1 action

move :: Int -> Int -> State Stacks ()
move fromStack toStack = do
    s <- use (ix (fromStack - 1))
    case uncons s of
        Just (top, remaining) -> do
            ix (toStack - 1) %= (top:)
            ix (fromStack - 1) .= remaining
        Nothing -> pure ()

moveN :: Action -> State Stacks ()
moveN (Action qty fromStack toStack) = replicateM_ qty (move fromStack toStack)

moveN2 :: Action -> State Stacks ()
moveN2 (Action qty fromStack toStack) = do
    s <- use (ix (fromStack - 1))
    let toMove = s ^.. taking qty traverse
        remaining = s ^.. dropping qty traverse
    ix (toStack - 1) %= (toMove <>)
    ix (fromStack - 1) .= remaining

main :: IO ()
main = parseAndSolve 5 actions (calculateBoth solve1 solve2)

solve1 :: Traversable t => t Action -> [Char]
solve1 moves = toList $ execState (traverse moveN moves) initial <&> head

solve2 :: Traversable t => t Action -> [Char]
solve2 moves = toList $ execState (traverse moveN2 moves) initial <&> head

type Stacks = Seq.Seq [Char]

initial :: Stacks
initial = Seq.fromList
    [['C'
    ,'Q'
    ,'B'
    ],
        
    ['Z'
    ,'W'
    ,'Q'
    ,'R'
    ],
        
    ['V'
    ,'L'
    ,'R'
    ,'M'
    ,'B'
    ],
        
    ['W'
    ,'T'
    ,'V'
    ,'H'
    ,'Z'
    ,'C'
    ],
    
    ['G'
    ,'V'
    ,'N'
    ,'B'
    ,'H'
    ,'Z'
    ,'D'
    ],
    
    ['Q'
    ,'V'
    ,'F'
    ,'J'
    ,'C'
    ,'P'
    ,'N'
    ,'H'
    ],
        
    ['S'
    ,'Z'
    ,'W'
    ,'R'
    ,'T'
    ,'G'
    ,'D'
    ],
    
    ['P'
    ,'Z'
    ,'W'
    ,'B'
    ,'N'
    ,'M'
    ,'G'
    ,'C'
    ],
    
    ['P'
    ,'F'
    ,'Q'
    ,'W'
    ,'M'
    ,'B'
    ,'J'
    ,'N'
    ]
    ]
