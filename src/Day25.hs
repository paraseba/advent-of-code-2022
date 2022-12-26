module Day25 where

import Control.Monad.Combinators.NonEmpty (sepEndBy1)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Extra (some1)
import Text.Megaparsec ((<|>))
import Text.Megaparsec.Char (char, eol)
import Utils

data Digit = Zero | One | Two | Minus | DoubleMinus
    deriving (Ord, Eq, Show)

type Snafu = NonEmpty Digit

numbersP :: Parser (NonEmpty Snafu)
numbersP = sepEndBy1 snafu eol
  where
    snafu = some1 digit
    digit =
        Zero <$ char '0'
            <|> One <$ char '1'
            <|> Two <$ char '2'
            <|> Minus <$ char '-'
            <|> DoubleMinus <$ char '='

snafu2int :: Snafu -> Integer
snafu2int = foldl go 0
  where
    go res digit = res * 5 + digit2int digit

int2snafu :: Integer -> Snafu
int2snafu 0 = Zero :| []
int2snafu 1 = One :| []
int2snafu 2 = Two :| []
int2snafu 3 = One :| [DoubleMinus]
int2snafu 4 = One :| [Minus]
int2snafu n
    | r == 0 = int2snafu (n `div` 5) <> (Zero :| [])
    | otherwise =
        int2snafu ((n - subs r) `div` 5) <> NE.singleton (NE.last (int2snafu r))
  where
    r = n `mod` 5
    subs 3 = -2
    subs 4 = -1
    subs x = x

digit2int :: Digit -> Integer
digit2int Zero = 0
digit2int One = 1
digit2int Two = 2
digit2int Minus = -1
digit2int DoubleMinus = -2

showSnafu :: Snafu -> String
showSnafu = map dig2char . toList
  where
    dig2char Zero = '0'
    dig2char One = '1'
    dig2char Two = '2'
    dig2char Minus = '-'
    dig2char DoubleMinus = '='

main :: IO ()
main = parseAndSolveIO 25 numbersP solve1 solve2

solve1 :: (Foldable t, Functor t) => t Snafu -> IO ()
solve1 input = putStrLn $ showSnafu . int2snafu . sum $ snafu2int <$> input

solve2 :: p1 -> p2 -> IO ()
solve2 _ _ = putStrLn  "And that's a wrap"
