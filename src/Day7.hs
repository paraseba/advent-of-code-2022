{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day7 where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (void)
import Control.Monad.State.Strict (State, execState)
import Data.Char (isPrint)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty, some1)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Text.Megaparsec (eof, takeWhileP)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils

data DirSelect
    = Root
    | Named Text
    | Parent
    deriving (Eq, Show)

data Command
    = Cd DirSelect
    | Ls
    deriving (Eq, Show)

data Node
    = FileNode Integer Text
    | DirNode Text
    deriving (Eq, Show)

end :: Parser ()
end = void newline <|> eof

name :: Parser Text
name = takeWhileP Nothing isPrint

command :: Parser Command
command = string "$ " *> (ls <|> cd) <* end
  where
    ls = string "ls" $> Ls
    cd = string "cd " *> (Cd <$> dir)
    dir =
        (string "/" $> Root)
            <|> (string ".." $> Parent)
            <|> (Named <$> name)

node :: Parser Node
node = (file <|> dir) <* end
  where
    file = FileNode <$> decimal <*> (char ' ' *> name)
    dir = DirNode <$> (string "dir " *> name)

session :: Parser (NonEmpty (Either Command Node))
session = some1 (Left <$> command <|> Right <$> node)

type DirPath = [Text]

data SessionState = SessionState
    { _currentDir :: DirPath
    , _dirSizes :: (Map.Map DirPath Integer)
    }
    deriving (Show)

initialState :: SessionState
initialState = SessionState [] Map.empty

makeLenses ''SessionState

cd :: DirSelect -> State SessionState ()
cd Root = currentDir .= []
cd Parent = currentDir %= tail
cd (Named n) = currentDir %= (n :)

addItem :: Node -> State SessionState ()
addItem (DirNode _) = pure ()
addItem (FileNode size _) = do
    current <- use currentDir
    traverse_ addToDir (tails current)
  where
    addToDir dir = modifying (dirSizes . at dir . non 0) (+ size)

logSessionItem :: Either Command Node -> State SessionState ()
logSessionItem (Left Ls) = pure ()
logSessionItem (Left (Cd dir)) = cd dir
logSessionItem (Right n) = addItem n

computeSizes :: Traversable t => t (Either Command Node) -> Map.Map DirPath Integer
computeSizes ses =
    initialState
        & execState (traverse logSessionItem ses)
        & view dirSizes

solve1 :: Traversable t => t (Either Command Node) -> Integer
solve1 ses = computeSizes ses & sumOf (folded . filtered (< 100000))

solve2 :: Traversable t => t (Either Command Node) -> Maybe Integer
solve2 ses = minimumOf (folded . filtered (> toFree)) sizes
  where
    sizes = computeSizes ses
    used = sizes ^. (at [] . non 0)
    unused = 70000000 - used
    needed = 30000000
    toFree = needed - unused

main :: IO ()
main = parseAndSolve 7 session (calculateBoth solve1 solve2)
