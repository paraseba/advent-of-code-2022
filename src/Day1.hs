module Day1 where
import Data.Void (Void)
import Data.Text qualified as T
import Text.Megaparsec hiding (sepBy1)
import Text.Megaparsec.Char
import Data.List.NonEmpty (NonEmpty, some1, sortBy)
import Data.Functor (void)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Applicative.Combinators.NonEmpty (sepBy1)
import qualified Data.Text.IO as TIO
import Control.Lens

type Parser = Parsec Void T.Text

item :: Parser Int
item = decimal

type ElfInventory = NonEmpty Int
elfInventory :: Parser ElfInventory
elfInventory = some1 (item <* (void eol <|> eof))

type Inventory = NonEmpty ElfInventory
inventory :: Parser Inventory
inventory = elfInventory `sepBy1` eol



main :: IO ()
main = do
    input <- TIO.readFile "day1.txt"
    case parse inventory "day1.txt" input of
         Left err -> putStrLn (errorBundlePretty err)
         Right inv -> do
            print $ calculate1 inv
            print $ calculate2 inv
    putStrLn "Done"


calculate1 :: Inventory -> Int
calculate1 = maximum1Of (folded . to (sumOf folded))

calculate2 :: Inventory -> Int
calculate2 inv = 
    inv
    & mapped %~ (sumOf folded)
    & sortBy (flip compare)
    & sumOf (taking 3 traverse)
