{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day17 where

import Control.Applicative ((<|>))
import Control.Lens
import Data.Bits (popCount, setBit, shiftL, shiftR, testBit, zeroBits, (.&.), (.|.))
import Data.Either.Extra (fromLeft)
import Data.Foldable (foldl', toList)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, some1)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word8)
import Text.Megaparsec.Char (char)
import Utils

jetsP :: Parser (NonEmpty Dir)
jetsP = some1 (DL <$ char '<' <|> DR <$ char '>')

data Dir = DL | DR
    deriving (Eq, Show)

type CaveOcc = Seq.Seq Word8

type Rock = [Word8]

data CaveState = CaveState
    { caveOcc :: CaveOcc
    , caveRocks :: [Rock]
    , currentRock :: Rock
    , caveJets :: [Dir]
    , currentPos :: Int
    , numRocks :: Int
    }

initialPos :: Int
initialPos = -4

stepSim :: CaveState -> CaveState
stepSim st =
    let shifted = shiftState st
        moved = lower shifted
     in fromMaybe (reset shifted) moved
  where
    reset sh@CaveState{..} =
        sh
            { caveRocks = tail caveRocks
            , currentRock = head (tail caveRocks)
            , currentPos = -4
            , caveOcc = fixCurrentRock sh
            , numRocks = numRocks + 1
            }

stepStone :: CaveState -> CaveState
stepStone st@CaveState{numRocks = old} =
    head $ dropWhile (\s -> numRocks s == old) $ iterate stepSim st

fixCurrentRock :: CaveState -> CaveOcc
fixCurrentRock CaveState{..} =
    let rows = zipWith (.|.) (caveRows currentRock currentPos caveOcc) currentRock
        n = length rows
        setOcc (row, i) occ
            | i >= 0 = Seq.update i row occ
            | otherwise = row <| occ
     in foldr setOcc caveOcc $ zip rows [currentPos - n + 1, currentPos - n + 2 .. currentPos]

lower :: CaveState -> Maybe CaveState
lower st@CaveState{..}
    | canFall st = Just $ st{currentPos = currentPos + 1}
    | otherwise = Nothing

shiftState :: CaveState -> CaveState
shiftState st@CaveState{..} =
    st
        { currentRock = shift currentRock (head caveJets) currentPos caveOcc
        , caveJets = tail caveJets
        }

canFall :: CaveState -> Bool
canFall CaveState{..} = canMoveTo currentRock (currentPos + 1) caveOcc

shift :: Rock -> Dir -> Int -> CaveOcc -> Rock
shift r dir y occ
    | canShift r dir y occ = shiftRock dir r
    | otherwise = r

shiftRock :: Dir -> Rock -> Rock
shiftRock DL r = flip shiftL 1 <$> r
shiftRock DR r = (\row -> shiftR row 1 .&. 0b11111110) <$> r

canShift :: Rock -> Dir -> Int -> CaveOcc -> Bool
canShift rock dir y occ = sum (popCount <$> shifted) == sum (popCount <$> rock) && canMoveTo shifted y occ
  where
    shifted = shiftRock dir rock

caveRows :: Rock -> Int -> CaveOcc -> [Word8]
caveRows r y occ = rowCave <$> [y - n + 1, y - n + 2 .. y]
  where
    n = length r
    rowCave :: Int -> Word8
    rowCave i
        | i >= 0 = occ `Seq.index` i
        | otherwise = freeRow

canMoveTo :: Rock -> Int -> CaveOcc -> Bool
canMoveTo rock y occ = and $ zipWith canMoveInto cave rock
  where
    cave = caveRows rock y occ
    canMoveInto :: Word8 -> Word8 -> Bool
    canMoveInto caveRow rockRow =
        popCount (rockRow .|. caveRow) == popCount rockRow + popCount caveRow

freeRow :: Word8
freeRow = zeroBits

toRock :: [String] -> Rock
toRock = fmap toWord
  where
    toWord :: String -> Word8
    toWord = foldl' char2bit freeRow . take 7 . (++ repeat ' ')
    char2bit w '#' = shiftL (setBit w 0) 1
    char2bit w _ = shiftL w 1

initialOcc :: CaveOcc
initialOcc = Seq.singleton $ 0b11111110

allRocks :: [Rock]
allRocks =
    toRock
        <$> [
                [ "  #### "
                ]
            ,
                [ "   #   "
                , "  ###  "
                , "   #   "
                ]
            ,
                [ "    #  "
                , "    #  "
                , "  ###  "
                ]
            ,
                [ "  #    "
                , "  #    "
                , "  #    "
                , "  #    "
                ]
            ,
                [ "  ##   "
                , "  ##   "
                ]
            ]

data SimState = SimState
    { ssNextRock :: Rock
    , ssTop :: (Vector Int)
    , ssJets :: [Dir]
    }
    deriving (Eq, Show)

caveState2simState :: Int -> CaveState -> SimState
caveState2simState numJets CaveState{..} =
    SimState
        { ssJets = take numJets caveJets
        , ssNextRock = currentRock
        , ssTop = fromLeft undefined getTop
        }
  where
    getTop = ifoldlM go (V.replicate 7 (-1)) (caveOcc |> 0)
    go i res w =
        if done res
            then Left res
            else Right $ res V.// ([1 .. 7] >>= (\b -> if testBit w b && res V.! (b - 1) < 0 then [(b - 1, i)] else []))

    done :: Vector Int -> Bool
    done = all (>= 0)

main :: IO ()
main = parseAndSolveIO 17 jetsP solve1 solve2

printCave :: CaveState -> IO ()
printCave CaveState{..} = printOcc *> printHeight *> printPos *> printRock
  where
    printRock = putStrLn $ "current:\n" <> (intercalate "\n" $ rowToString <$> toList currentRock)
    printOcc = putStrLn $ intercalate "\n" $ (rowToString <$> toList caveOcc)
    printHeight = putStrLn $ "height = " <> show (length caveOcc - 1)
    printPos = putStrLn $ "position = " <> show currentPos
    rowToString row = [7, 6 .. 1] >>= toChar . testBit row
    toChar True = "#"
    toChar False = "."

initial :: Foldable t => t Dir -> CaveState
initial jets =
    CaveState
        { caveOcc = initialOcc
        , caveRocks = cycle allRocks
        , caveJets = cycle (toList jets)
        , currentPos = initialPos
        , currentRock = head allRocks
        , numRocks = 0
        }

solve1 :: Foldable t => t Dir -> IO ()
solve1 jets = do
    let st = head $ dropWhile ((< targetRocks) . numRocks) (iterate stepSim (initial jets))
    putStrLn $ show $ length (caveOcc st) - 1
  where
    targetRocks = 2022

solve2 :: Foldable t => p -> t Dir -> IO ()
solve2 _ jets = do
    let st = drop 1 $ zip (iterate stepStone (initial jets)) (iterate (stepStone . stepStone) (initial jets))
        (rep1, rep2) = head $ dropWhile (\(s1, s2) -> caveState2simState n s1 /= caveState2simState n s2) st
        n = length jets
        nR1 = numRocks rep1
        h1 = length (caveOcc rep1) - 1
        nR2 = numRocks rep2
        h2 = length (caveOcc rep2) - 1
        (sections, extra) = (target - nR1) `divMod` (nR2 - nR1)
        res1 = sections * (h2 - h1)
        extraSim1 = length $ caveOcc $ head $ dropWhile (\s -> numRocks s < extra) (iterate stepStone rep1{numRocks = 0 {- (initial jets) -}})
        extraSim = extraSim1 - 1

    print $ res1 + extraSim
  where
    target = 1_000_000_000_000
