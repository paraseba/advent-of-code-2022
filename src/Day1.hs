module Day1 where

import Text.Megaparsec hiding (sepBy1)
import Text.Megaparsec.Char
import Data.List.NonEmpty (NonEmpty, some1, sortBy)
import Data.Functor (void)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Control.Lens
import Utils (Parser, parseAndSolve)


item :: Parser Int
item = decimal

type ElfInventory = NonEmpty Int
elfInventory :: Parser ElfInventory
elfInventory = some1 (item <* (void eol <|> eof))

type Inventory = NonEmpty ElfInventory
inventory :: Parser Inventory
inventory = elfInventory `sepBy1` eol

solve1 :: Inventory -> Int
solve1 = maximum1Of (folded . to (sumOf folded))

solve2 :: prev -> Inventory -> Int
solve2 _ inv = 
    inv
    & mapped %~ (sumOf folded)
    & sortBy (flip compare)
    & sumOf (taking 3 traverse)

main :: IO ()
main = parseAndSolve 1 inventory solve1 solve2

