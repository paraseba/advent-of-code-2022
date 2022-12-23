module Day20 where

import Control.Lens
import Control.Monad.Combinators.NonEmpty (sepEndBy1)
import Data.Foldable (foldl', toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromJust)
import Data.Sequence qualified as Seq
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Utils

numbersP :: Parser (NonEmpty Int)
numbersP = sepEndBy1 (signed (pure ()) decimal) eol

main :: IO ()
main = parseAndSolveIO 20 numbersP solve1 solve2

newtype Position = Pos {unPos :: Int}
    deriving (Eq, Ord, Num, Enum, Show)

type Nums = Seq.Seq (Int, Position)

findPosition :: Seq.Seq (a, Position) -> Position -> Int
findPosition ns i = fromJust $ Seq.findIndexL ((== i) . snd) ns

move :: Nums -> Position -> Nums
move ns i = (Seq.take newIx base |> (nn, nix)) <> Seq.drop newIx base
  where
    i' = findPosition ns i
    (nn, nix) = Seq.index ns i'
    len = length ns
    newIx' = (i' + nn) `mod` (len - 1)
    newIx = if newIx' == 0 then len - 1 else newIx'
    base = Seq.take i' ns <> Seq.drop (i' + 1) ns

addPos :: Foldable f => f Int -> Nums
addPos ns = Seq.fromList (zip (toList ns) [0 ..])

findNum :: Eq a => Seq.Seq (a, Position) -> a -> Int
findNum ns n = fromJust $ Seq.findIndexL ((== n) . fst) ns

idx :: Seq.Seq (a, b) -> Int -> a
idx ns i = fst $ Seq.index ns (i `mod` length ns)

decryptionKey :: Int
decryptionKey = 811589153

mix :: Int -> Seq.Seq (Int, Position) -> Nums
mix n ns =
    let poss = concat $ replicate n [0 .. fromIntegral (length ns) - 1]
     in foldl' move ns poss

findCoord :: (Eq a, Num a) => Seq.Seq (a, Position) -> a
findCoord mixed = 
    let zind = findNum mixed 0
        n1000 = idx mixed (zind + 1000)
        n2000 = idx mixed (zind + 2000)
        n3000 = idx mixed (zind + 3000)
    in n1000 + n2000 + n3000

solve1 :: NonEmpty Int -> IO ()
solve1 input = do
    print $ findCoord (mix 1 ns)
  where
    ns :: Nums
    ns = addPos input

solve2 :: prev -> NonEmpty Int -> IO ()
solve2 _ input = do
    print $ findCoord (mix 10 ns)
  where
    ns :: Nums
    ns = addPos ((* decryptionKey) <$> input)
