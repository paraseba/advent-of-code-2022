module Day13 where

import Control.Applicative ((<|>))
import Control.Lens hiding ((<|))
import Control.Monad (void)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty, (<|))
import Data.List.NonEmpty qualified as NE
import Numeric.Lens (adding)
import Text.Megaparsec (sepEndBy, sepEndBy1)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils
import Prelude hiding (pi)

data Packet
    = PI Int
    | PL [Packet]
    deriving (Show, Eq)

instance Ord Packet where
    compare :: Packet -> Packet -> Ordering
    compare (PI n) (PI m) = compare n m
    compare (PL ps) (PL qs) = compare ps qs
    compare (PI n) list@(PL _) = compare (PL [PI n]) list
    compare list@(PL _) (PI n) = compare list (PL [PI n])

packetP :: Parser Packet
packetP =
    PI <$> decimal
        <|> PL <$> (char '[' *> sepEndBy packetP (char ',') <* char ']')

commP :: Parser (NonEmpty (Packet, Packet))
commP = NE.fromList <$> sepEndBy1 packetPairP eol
  where
    packetPairP = (,) <$> (packetP <* void eol) <*> (packetP <* void eol)

main :: IO ()
main = parseAndSolve 13 commP solve1 solve2

solve1 :: NonEmpty (Packet, Packet) -> Int
solve1 =
    sumOf (folded . _1 . adding 1)
        . itoListOf (itraversed . to (uncurry compare) . filtered (== LT))

solve2 :: Int -> NonEmpty (Packet, Packet) -> Int
solve2 _ packets = search sep1 * search sep2
  where
    sep n = PL [PL [PI n]]
    sep1 = sep 2
    sep2 = sep 6
    withSep = (sep1, sep2) <| packets
    sorted = sort $ withSep ^.. (folded . both)
    search s = maybe 0 (+1) $ findIndexOf folded (== s) sorted

