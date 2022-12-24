{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day12 where

import Control.Lens hiding ((<|))
import Control.Monad.Except (MonadError, runExcept, throwError)
import Control.Monad.State.Strict (MonadState, evalStateT)
import Data.Char (ord)
import Data.Functor (($>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Utils
import Vec2d

type Path = [Coord]

data Size = Size Row Col
    deriving (Show)

data Point = Regular Coord | Exit Coord
    deriving (Eq, Show)

data NodeInfo = NodeInfo
    { _niPath :: Path
    , _niCoord :: Point
    }
    deriving (Show)
makeLenses ''NodeInfo

data TravelState = TravelState
    { _tsQueue :: Seq.Seq NodeInfo
    , _tsVisited :: Set Coord
    }
makeLenses ''TravelState

type HeightMap = Vec2d Char

main :: IO ()
main = solve solve1 solve2

neighbors :: Size -> Coord -> [Coord]
neighbors (Size nRows nCols) (row, col) =
    filter
        valid
        [ (row + 1, col)
        , (row - 1, col)
        , (row, col + 1)
        , (row, col - 1)
        ]
  where
    valid (r, c) = r >= 0 && c >= 0 && r < nRows && c < nCols

accessible :: HeightMap -> Size -> Coord -> [Point]
accessible hm s current =
    [ p | candidate <- (neighbors s current), let ch = hm ! candidate, let p = toPoint ch candidate, canGet ch
    ]
  where
    currentH = height (hm ! current)

    canGet :: Char -> Bool
    canGet ch = height ch - currentH <= 1

    toPoint :: Char -> Coord -> Point
    toPoint 'E' coord = Exit coord
    toPoint _ coord = Regular coord

    height :: Char -> Int
    height 'E' = (ord 'z') - (ord 'a')
    height 'S' = 0
    height other = (ord other) - (ord 'a')

findChar :: Char -> HeightMap -> [Coord]
findChar c hm = fst <$> itoListOf (itraversed . filtered (== c)) hm

popNode :: forall m. (MonadState TravelState m, MonadError Text m) => m NodeInfo
popNode =
    uses tsQueue uncons >>= \case
        Just (node, rest) -> (tsQueue .= rest) $> node
        Nothing -> throwError "No solutions"

findPath ::
    forall m.
    (MonadState TravelState m, MonadError Text m) =>
    HeightMap ->
    m Path
findPath hm = popNode >>= processNode hm

processNode ::
    forall m.
    (MonadState TravelState m, MonadError Text m) =>
    HeightMap ->
    NodeInfo ->
    m Path
processNode hm node = do
    case node ^. niCoord of
        Exit _ -> pure $ node ^. niPath
        Regular coord -> do
            visited <- use tsVisited
            if Set.member coord visited
                then findPath hm
                else
                    modifying tsVisited (Set.insert coord)
                        *> modifying tsQueue (Seq.>< newNodes coord)
                        *> findPath hm
  where
    size = Size (Row . numRows $ hm) (Col . numCols $ hm)
    newNodes coord =
        let neighs = accessible hm size coord
         in Seq.fromList $ (NodeInfo (coord : node ^. niPath)) <$> neighs

initState :: Coord -> TravelState
initState startCoord =
    TravelState (Seq.singleton startNode) Set.empty
  where
    startNode = NodeInfo [] (Regular startCoord)

run :: HeightMap -> TravelState -> Either Text Int
run hm initial = length <$> runExcept (evalStateT (findPath hm) initial)

theMap :: HeightMap
-- theMap = testMap
theMap = inputMap

solve1 :: IO ()
solve1 = putStrLn $ either T.unpack show $ run theMap (initState startCoord)
  where
    startCoord = findChar 'S' theMap ^?! folded

solve2 :: prev -> IO ()
solve2 _ = putStrLn $ maybe "No solutions" show $ minimumOf (folded . _Right) res
  where
    candidateStarts :: HeightMap -> [Coord]
    candidateStarts hm = findChar 'S' hm ++ findChar 'a' hm
    res = run theMap . initState <$> candidateStarts theMap

testMap :: HeightMap
testMap =
    fromList
        [ "Sabqponm" :: String
        , "abcryxxl"
        , "accszExk"
        , "acctuvwj"
        , "abdefghi"
        ]

inputMap :: HeightMap
inputMap =
    fromList
        [ "abccccccccaaaaccccaaacaccccaaaaaacccccccccccaaaccccccccccaaaaaaacccccccccccccccccccccccccccccacaaaccccccccccccccccccccccccccccccccccccccccaaaaa" :: String
        , "abccccccccaaaaccaaaaaaaacccaaaaaacccccccccccaaaacccccccccaaaaaaaaaacccccccccccccccaaccccccccaaaaaccaaaccaacccccccccccccccccccccccccccccccaaaaaa"
        , "abcccccccccaacccaaaaaaaaccccaaaaacccccccccccaaaacccccccaaaaaaaaaaaaaccccccccccaaaaaaccccccccaaaaaaaaaaaaaaccccccccccccccccaaaccccccccccccaaaaaa"
        , "abcccccccccccccccaaaaaccccccaacaaccccaacccccaaacccccccaaaaaaaaaaaaaaccccccccccaaaaaaacccccccccaaaaacaaaaaaccccccccccccccccaaccccccccccccccccaaa"
        , "abccccccccccccccccaaaaaccccccccccaaccaacccccccccccccccaaaaacaaaacacacccccaacccaaaaaaaacccccccaaaaacaaaaaaaccccccccccccccccaaacccccccccccccccaaa"
        , "abcccccccccccccccaaaaaaccccccccccaaaaaaccccccccccccccccaaaaaaaacaaaaacaaaaaccccaaaaaaacccccccaacaacaaaaaaaaccccccccaaaaccaakcccccccccccccccccaa"
        , "abcccccccccccccccaaaccacccccccccccaaaaaaacccccccaaaccccccaaaaaacaaaaaccaaaaaccaaaaaaccccccccccccaacaaaaaaaacccccccccaaaakkkklllcccccccccccccccc"
        , "abcccccaaaccccccccccccccccccccccccaaaaaaacccccccaaacacccaaaaaaaaaaaaacaaaaaaccaaaaaacccccccccccccccccaaaccccccccccccaaakkkkkllllcccccccaacccccc"
        , "abccccaaaacccccccccccccccccccccccaaaaaacccccccaccaaaaaccaaaaaaaaaaaaacaaaaccccccccaaccccccccccccccccccaaccccccccccccckkkkkkpllllccccaaaaaaccccc"
        , "abccccaaaacccccccccccccccccaaacccaaaaaacccccccaaaaaaaacccaaaaacaaaaaacccaaaccccccccccccccccccccccccccccccccccccccccckkkkpppppplllcccaaaaacccccc"
        , "abcccccaaaccccccccccccccccaaaacccccccaaccccccccaaaaacccccaaaccccaaacccccccccccccccccccccccccaaccccccccccccccccccjjjkkkkpppppppplllcccaaaaaacccc"
        , "abccccccccccccccccccccccccaaaaccccccccccccccccccaaaaacccccccccccccccccccccccccccccccccccccccaaaaaccccccccccccjjjjjjkkkrppppuppplllccccaaaaacccc"
        , "abccccccccccccccaaaccccccccaaaccccccccccccccccccaacaaccccccccccccccccccccccaaaccccccccaacccaaaaaccccccccccccjjjjjjjjrrrpuuuuuppplllcccccaaacccc"
        , "abcccccaaccaacccaaacacccccccccccccccccccccccccacaaaaccccccccccccccccccccccaaaaaaccccaaaacccaaaaaaccaccccccccjjjrrrrrrrrruuuuuppplllmcccddcccccc"
        , "abcccccaaaaaacaaaaaaaaccccccccccccccccccccccccaacaaaccccccccccccccccccccccaaaaaaccccaaaaaacccaaaaccaaacaaacjjjrrrrrrrrruuuxuuupqqlmmmmddddccccc"
        , "abcccccaaaaaccaaaaaaaaccccccccccccccccccccccccaaaaaccccaacccccccccccccccccaaaaaacccccaaaacccaacccccaaaaaaacjjjrrrrtuuuuuuxxyuvqqqqmmmmmddddcccc"
        , "abaacccaaaaaaccaaaaaccccccccccccccccccaaaaccccaaaaaaccaaaccccccccccccccccccaaaaaccccaaaaaccccccccccaaaaaaccjjjrrrtttuuuuuxxyvvvqqqqqmmmmdddcccc"
        , "abaaccaaaaaaaaccaaaaaccccccccccccccccaaaaaaaaaaaaaaaacaaacaaaccccccccccccccaacaacaacaacaaccccccccaaaaaaaaccijjqqrtttxxxxxxyyvvvvvqqqqmmmmdddccc"
        , "abaaccaaaaaaaacaaaaaaccccccccccccccccaaaaaaaaaaaaaaaaaaaaaaaacccccccccccccccaaacaaaccccccccccaaccaaaaaaaaaciiiqqqttxxxxxxxyyvvvvvvvqqqmmmdddccc"
        , "abaaaccccaaccccaaaccacccccccccccccccccaaaaaaccccaacaaaaaaaaaaccaaaccccccccccaaaaaaaccccccccccaaaaaaaaaaaaaaiiiqqqtttxxxxxxyyyyyyvvvqqqmmmdddccc"
        , "SbaaaccccaacccccccccccccccccccccccccaaaaaaaaccccaacccaaaaaaccaaaaaaccccccccccaaaaaacccccccccccaaaaacaaacaaaaiiiqqqttxxxxEzzyyyyyvvvqqqmmmdddccc"
        , "abaaaccccccccccccccccccccccccccccccaaaaaaaaaaccccccccaaaaaaccaaaaaaccccccccccaaaaaaaaccccccccaaaaaacaaaccaaaiiiqqqtttxxxyyyyyyvvvvqqqmmmdddcccc"
        , "abaccccccaacccccccccccccccccccccccccaaaaaaaaaaaacccccaaaaaaacaaaaaacccccccccaaaaaaaaacccccccaaaaaaaacaaaaaaaiiiqqqtttxxyyyyyyvvvvqqqqnnmeddcccc"
        , "abccccccaaaaccccccccccccaaaccccccccccccaaaaaaaaaaacccaaacaaacaaaaacccccccccaaaaaaaaaacccccccaaaaaaaaccaaaaaaaiiiqqtttxxyyyyyywvrrrrnnnneeeccccc"
        , "abccccccaaaacccccaacccccaaaacccccccccccaaaccaaaaaacccacccccccaaaaacccccccccaaacaaacccccccccccccaacccccccaaaaaiiqqqttxxwywwyyywwrrrnnnneeeeccccc"
        , "abccccccaaaaccaacaaaccccaaaaccccccaacccaacccaaaaaccccccccccccccccccccccccccccccaaacccccccccccccaaccccccaaaaaaiiqqqttwwwwwwwwywwrrrnnneeeecccccc"
        , "abccccccccccccaaaaacccccaaaccccacaaacccccccccaaaaacccccccccccccccccccccccccccccaaacccccccccccccccccccccaaaaaaiiqqpsswwwwsswwwwwrrnnneeeeccccccc"
        , "abcccccccccccccaaaaaacccccccccaaaaacaacccccccaacaacccccaaccccccccccccccccccccccccccccccccccccccccccccccaccaahhhpppssssssssswwwwrrnnneeeaccccccc"
        , "abcccccccccccaaaaaaaacccccccccaaaaaaaacccccaaccccccaaacaaccccccccccccccccccccccccccccccccccccaaaccaccccccccchhhpppsssssssssswwrrrnnneeeaaaacccc"
        , "abcccccccccccaaaaacaacccccccccccaaaaaccaaaaaaccccccaaaaaaccccccccccccccccccccccccccccaaccaaccaaaaaacccccccccchhpppppsspppossrrrrrnnneeeaaaacccc"
        , "abccccccccccccacaaaccccccccccccaaaaacccaaaaaaaacccccaaaaaaaccaaaccccccccaaaacccccccccaaaaaacccaaaaacccccccccchhhpppppppppoosrrrroonffeaaaaacccc"
        , "abccccccccccccccaaaccccccccccccaacaaaccaaaaaaaaccccaaaaaaaaacaaaccccccccaaaacccccccccaaaaaccaaaaaaacccccccccchhhhpppppppoooooooooonfffaaaaccccc"
        , "abcccccccccccccccccccccccaaaccccccaaccccaaaaaaaccccaaaaaaaaaaaaaaaccccccaaaacccccccccaaaaaacaaaaaaaacccccaacchhhhhhhhgggggoooooooofffaaaaaacccc"
        , "abcccccccccccccccccccccccaaaaacccaacccccaaaaaccccccaaaaaacaaaaaaaaccccccaaacccccccccaaaaaaaaaaaaaaaaccccaaacccchhhhhgggggggooooooffffaaaaaacccc"
        , "abccaaacccccccccccccccccaaaaaaccaaaccccaaaaaacccccccccaaacccaaaaacccccccccccccccccccaaaaaaaaccaaacacccccaaacaaachhhggggggggggfffffffcaacccccccc"
        , "abcaaaaccccccccccaacccccaaaaaaccaaacaaaccccaaccccccccccccccaaaaacccccccccccccccccccccccaacccccaaaccccaaaaaaaaaacccccccccaagggffffffcccccccccccc"
        , "abcaaaaccccccccccaaccccccaaaaaaaaaaaaaaccccccccccccccccccccaaaaaaccccccccccccccccccccccaaccccccccccccaaaaaaaaaccccccccccaaacgfffffcccccccccccaa"
        , "abccaaacccccccaaaaaaaacccaaaaaaaaaaaaaaccccccccaaaaaccaaaaaaaaaaaaaacaacccccccccaaaccacccccccccccccccccaaaaacccccccccccaaaaccccccccccccccccccaa"
        , "abccccccccccccaaaaaaaacccccccccaaaaaaccccccccccaaaaaccaaaaaaaaaaaaaacaaaaaacccccaaaaaacccccccccccccccccaaaaaacccccccccccaacccccccccccccccacacaa"
        , "abccccccccccccccaaaacccccccccccaaaaaacccccccccaaaaaacccaaaaaaaaaaaaacaaaaaacccccaaaaaacccccccccccccccccaaaaaaaccccccccccaacccccccccccccccaaaaaa"
        , "abcccccccccccccaaaaaaccccccccccaaaaaaaccccccccaaaaaaccccccaaaaaacccaaaaaaaaccccaaaaaaaaccccccccccccccccaaacaaacccccccccccccccccccccccccccaaaaaa"
        ]
