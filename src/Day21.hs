{-# LANGUAGE OverloadedStrings #-}

module Day21 where

import Control.Lens
import Control.Monad.Combinators.NonEmpty (sepEndBy1)
import Data.Char (isLetter)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Text.Megaparsec (takeWhileP, try, (<|>))
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils

data Expr
    = EInt Int
    | EVar Text
    | ESum Expr Expr
    | EMul Expr Expr
    | EDiv Expr Expr
    | ESub Expr Expr
    deriving (Show)

expP :: Parser Expr
expP = eint <|> try esum <|> try emul <|> try ediv <|> esub
  where
    ref :: Parser Text
    ref = takeWhileP Nothing isLetter
    bin f c = f <$> (EVar <$> (ref <* string " " <* char c <* string " ")) <*> (EVar <$> ref)
    eint = EInt <$> decimal
    esum = bin ESum '+'
    emul = bin EMul '*'
    ediv = bin EDiv '/'
    esub = bin ESub '-'

assignmentP :: Parser (Text, Expr)
assignmentP = (,) <$> (takeWhileP Nothing isLetter <* string ": ") <*> expP

planP :: Parser (NonEmpty (Text, Expr))
planP = sepEndBy1 assignmentP eol

eval :: M.Map Text Expr -> Expr -> Int
eval _ (EInt n) = n
eval env (ESum a b) = eval env a + eval env b
eval env (EMul a b) = eval env a * eval env b
eval env (EDiv a b) = eval env a `div` eval env b
eval env (ESub a b) = eval env a - eval env b
eval env (EVar name) = eval env (env M.! name)

data Sus = Unknown (Int -> Int) | Known Int

eval2 :: M.Map Text Expr -> Expr -> Sus
eval2 _ (EInt n) = Known n
eval2 env (ESum a b) = plus (eval2 env a) (eval2 env b)
eval2 env (EMul a b) = times (eval2 env a) (eval2 env b)
eval2 env (EDiv a b) = sdiv (eval2 env a) (eval2 env b)
eval2 env (ESub a b) = ssub (eval2 env a) (eval2 env b)
eval2 _ (EVar "humn") = Unknown id
eval2 env (EVar name) = eval2 env (env M.! name)

plus :: Sus -> Sus -> Sus
plus (Unknown f) (Known n) = Unknown (\x -> f (x - n))
plus (Known n) (Unknown f) = Unknown (\x -> f (x - n))
plus (Known n) (Known m) = Known (m + n)
plus _ _ = error "Internal error plus"

times :: Sus -> Sus -> Sus
times (Unknown f) (Known n) = Unknown (\x -> f (x `div` n))
times (Known n) (Unknown f) = Unknown (\x -> f (x `div` n))
times (Known n) (Known m) = Known (n * m)
times _ _ = error "Internal error times"

sdiv :: Sus -> Sus -> Sus
sdiv (Unknown f) (Known n) = Unknown (\x -> f (x * n))
sdiv (Known n) (Unknown f) = Unknown (\x -> f (n `div` x))
sdiv (Known n) (Known m) = Known (n `div` m)
sdiv _ _ = error "Internal error sdiv"

ssub :: Sus -> Sus -> Sus
ssub (Unknown f) (Known m) = Unknown (\x -> f (x + m))
ssub (Known n) (Unknown f) = Unknown (\x -> f (n - x))
ssub (Known n) (Known m) = Known (n - m)
ssub _ _ = error "Internal error ssub"

main :: IO ()
main = parseAndSolveIO 21 planP solve1 solve2

solve1 :: Foldable t => t (Text, Expr) -> IO ()
solve1 input = do
    print $ eval env (env M.! "root")
  where
    env = M.fromList (toList input)

solve2 :: Foldable t => prev -> t (Text, Expr) -> IO ()
solve2 _ input = do
    print $ case eval2 env r1 of
        Unknown f -> f eqTo
        Known n -> error $ "Unexpected error " <> show n
  where
    env =
        M.fromList (toList input)
            & M.delete "humn"
    eqTo = eval env r2
    (r1, r2) = case env M.! "root" of
        ESum a b -> (a, b)
        EMul a b -> (a, b)
        EDiv a b -> (a, b)
        ESub a b -> (a, b)
        _ -> error "Bad input"
