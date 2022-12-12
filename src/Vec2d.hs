{-# LANGUAGE TypeFamilies #-}

module Vec2d where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Lens

data Vec2d a = Vec2d (Vector a) Int
  deriving (Functor, Foldable, Traversable, Show)

newtype Row = Row Int deriving (Eq, Ord, Enum, Num, Show)
newtype Col = Col Int deriving (Eq, Ord, Enum, Num, Show)

type Coord = (Row, Col)


(!) :: Vec2d a -> Coord -> a
vec@(Vec2d as _) ! pos = as V.! index2d vec pos
{-# INLINE (!) #-}

infixl 9 !

index2d :: Vec2d a -> Coord -> Int
index2d (Vec2d _ rowSize) (Row row, Col col) = row * rowSize + col
{-# INLINE index2d #-}

fromList :: [[a]] -> Vec2d a
fromList [] = Vec2d V.empty 0
fromList rows@(as:_) = Vec2d backend (length as)
    where backend = V.fromList (rows ^.. (folded.folded))

singleton :: a -> Vec2d a
singleton a = Vec2d (V.singleton a) 1

withinBounds :: Vec2d a -> (Row, Col) -> Bool
withinBounds (Vec2d backend rowSize) (Row row, Col col) =
    0 <= row && row < rowSize && 0 <= col && col < V.length backend

numCols :: Vec2d a -> Int
numCols (Vec2d _ s) = s
{-# INLINE numCols #-}

numRows :: Vec2d a -> Int
numRows vec@(Vec2d backend _) = length backend `div` numCols vec
{-# INLINE numRows #-}

(//) :: Vec2d a -> [(Coord, a)] -> Vec2d a
vec@(Vec2d backend size) // values = Vec2d new size
  where new = backend V.// (values & (traversed._1) %~ index2d vec)
{-# INLINE (//) #-}

type instance Index (Vec2d a) = Coord
type instance IxValue (Vec2d a) = a

instance Ixed (Vec2d a) where 
    ix :: Coord -> Traversal' (Vec2d a) a
    ix coord f vec@(Vec2d backend rowSize)
      | withinBounds vec coord =
        f (vec ! coord) <&> \a -> Vec2d (backend V.// [(index2d vec coord, a)]) rowSize
      | otherwise = pure vec
      

instance FunctorWithIndex Coord Vec2d
instance FoldableWithIndex Coord Vec2d

instance TraversableWithIndex Coord Vec2d where
   itraverse :: Applicative f => (Coord -> a -> f b) -> Vec2d a -> f (Vec2d b) 
   itraverse f (Vec2d backend rowSize) =
      Vec2d <$> traverse (uncurry f) withCoords <*> pure rowSize
      where
        coords = V.generate (V.length backend) toCoord
        toCoord i = (Row $ i `div` rowSize , Col $ i `mod` rowSize)
        withCoords = V.zip coords backend

instance Applicative Vec2d where
    pure a = Vec2d (pure a) 1

    Vec2d fs fsize <*> Vec2d as asize = Vec2d (fs <*> as) (fsize * asize)

{- Applicative laws
 - 
 - pure id <*> v = v
 -   Vec2d (V.singleton id) 1 <*> Vec2d v cols = Vec2d v (1 * cols)
 -
 -
 - pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
 -   Vec2d (V.singleton (.)) 1 <*> u <*> v <*> w = Vec2d (V.singleton (.) <*> u) usize <*> v <*> w
 -   = Vec2d (V.pure (.) <*> u <*> v <*> w) (usize * vsize * wsize) = Vec2d ( uvec <*> (vvec <*> wvec) ) (usize * vsize * wsize)
 -   = u <*> (v <*> w)
 -
 -
 - pure f <*> pure x = pure (f x)
 - Vec2d (pure f) 1 <*> Vec2d (pure x) 1 = Vec2d (pure f <*> pure x) 1 = Vec2d (pure (f x)) 1 = pure (f x)
 -
 -
 -
 - u <*> pure y = pure ($ y) <*> u
 - Vec2d (uvec) usize <*> Vec2d (pure y) 1 = Vec2d (uvec <*> pure y) usize
 - = Vec2d (pure ($ y) <*> uvec) usize
 - = pure ($ y) <*> Vec2d uvec usize
 - = pure ($ y) <*> u
 -}

