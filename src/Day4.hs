module Day4 where
import Utils
import Data.List.NonEmpty (NonEmpty, some1)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Functor (void)
import Text.Megaparsec.Char (eol, char)
import Control.Applicative ((<|>))
import Text.Megaparsec (eof)
import Control.Lens

assignments :: Parser (NonEmpty ((Int, Int), (Int, Int)))
assignments = some1 (pair <* (void eol <|> eof))

pair :: Parser ((Int, Int), (Int, Int))
pair = (\a b c d -> ((a,b),(c,d)))
       <$> decimal <* char '-'
       <*> decimal <* char ','
       <*> decimal <* char '-'
       <*> decimal

main :: IO ()
main = parseAndSolve 4 assignments (calculateBoth solve1 solve2)

containedIn :: Ord a => (a, a) -> (a, a) -> Bool
containedIn (i1, i2) (o1, o2) = o1 <= i1 && i2 <= o2

contained :: ((Int, Int), (Int, Int)) -> Bool
contained (a, b) = containedIn a b || containedIn b a

overlapping :: ((Int, Int), (Int, Int)) -> Bool 
overlapping ((a,b),(c,d)) =
    if a < c
    then b >= c
    else a <= d

solve1 :: NonEmpty ((Int, Int), (Int, Int)) -> Int
solve1 = lengthOf (folded.filtered contained)

solve2 :: NonEmpty ((Int, Int), (Int, Int)) -> Int
solve2 = lengthOf (folded.filtered overlapping)
