module Data.Counter
  ( Counter(toMap)
  , singleton
  , fromCounts
  , fromList
  , fromSet
  , fromMap
  , toList
  , increment
  , lookup
  , total
  , unsafeFromMap
  , valid ) where

import Control.Applicative
import Data.Binary
import Data.Default
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Set (Set)
import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

newtype Counter a = Counter { toMap :: Map a Integer }
  deriving (Show, Read, Eq, Binary)

instance Default (Counter a) where def = Counter Map.empty

instance Ord a => Monoid (Counter a) where
  mempty = def
  Counter m `mappend` Counter n =
    Counter (Map.unionWith (+) m n)

-- |
-- > singleton "x" == fromCounts [("x", 1)]
-- > singleton "x" == fromList ["x"]
singleton :: Ord a => a -> Counter a
singleton k = Counter $ Map.singleton k 1

-- |
-- > lookup "y" (fromCounts [("x", 1), ("y", 2)]) == 2
-- > lookup "z" (fromCounts [("x", 1), ("y", 2)]) == 0
lookup :: Ord a => a -> Counter a -> Integer
lookup x (Counter m) = fromMaybe 0 $ Map.lookup x m

-- |
-- > fromMap (Map.fromListWith (+) xs) == fromCounts xs
fromMap :: Map a Integer -> Counter a
fromMap = Counter . Map.filter (> 0)

unsafeFromMap :: Map a Integer -> Counter a
unsafeFromMap = Counter

fromCounts :: Ord a => [(a, Integer)] -> Counter a
fromCounts = Counter . Map.filter (> 0) . Map.fromListWith (+)

toList :: Ord a => Counter a -> [(a, Integer)]
toList (Counter m) = Map.toList m

-- |
-- > fromList ["x", "y", "z"] == fromCounts [("x", 1), ("y", 1), ("z", 1)]
fromList :: Ord a => [a] -> Counter a
fromList = fromCounts . map (\x -> (x, 1))

-- |
-- > fromSet xs == fromList . Set.toList
fromSet :: Ord a => Set a -> Counter a
fromSet = Counter . Map.fromAscList . map ((,) <$> id <*> const 1) . Set.toAscList

increment :: Ord a => a -> Counter a -> Counter a
increment x (Counter m) = Counter $ Map.insertWith (+) x 1 m

-- | Total number of entries in the 'Counter'.
total :: Counter a -> Integer
total = Map.foldr (+) 0 . toMap

-- | Check if the 'Counter' is valid (i.e. no negative values)
valid :: Counter a -> Bool
valid = Map.null . Map.filter (<= 0) . toMap
