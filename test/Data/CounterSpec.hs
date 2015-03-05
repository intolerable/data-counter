module Data.CounterSpec where

import Control.Applicative
import Data.Counter (Counter)
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Counter as Counter

instance (Arbitrary a, Ord a) => Arbitrary (Counter a) where
  arbitrary = Counter.fromList <$> arbitrary
  shrink c = Counter.fromCounts <$> shrink (Counter.toList c)

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = Set.fromList <$> arbitrary
  shrink s = Set.fromList <$> shrink (Set.toList s)

instance (Arbitrary a, Ord a, Arbitrary b) => Arbitrary (Map a b) where
  arbitrary = Map.fromList <$> arbitrary
  shrink m = Map.fromList <$> shrink (Map.toList m)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Counter" $ do

    describe "Monoid" $ do
      prop "mempty <> x == x" $ \(x :: Counter Int) ->
        mempty <> x == x
      prop "x <> mempty == x" $ \(x :: Counter Int) ->
        x <> mempty == x
      prop "x <> (y <> z) == (x <> y) <> z" $ \(x :: Counter String) y z ->
        x <> (y <> z) == (x <> y) <> z

    describe "Arbitrary" $ do
      prop "valid x" $ \(x :: Counter String) -> Counter.valid x
      prop "all valid (shrink x)" $ \(x :: Counter String) ->
        all Counter.valid (shrink x)

    describe "fromMap" $ do
      prop "fromMap (Map.fromListWith (+) xs) == fromCounts xs" $ \(xs :: [(Int, Integer)]) ->
        Counter.fromMap (Map.fromListWith (+) xs) == Counter.fromCounts xs
      prop "fromMap . toMap == id" $ \(x :: Counter String) ->
        Counter.fromMap (Counter.toMap x) == x
      prop "toMap . fromMap == id" $ \(x :: Map String Integer) ->
        Counter.toMap (Counter.fromMap x) == Map.filter (> 0) x

    describe "singleton" $ do
      prop "singleton x == fromCounts [(x, 1)]" $ \(x :: String) ->
        Counter.singleton x == Counter.fromCounts [(x, 1)]
      prop "singleton x == fromList [x]" $ \(x :: String) ->
        Counter.singleton x == Counter.fromList [x]
      prop "singleton x == fromSet (Set.singleton x)" $ \(x :: String) ->
        Counter.singleton x == Counter.fromSet (Set.singleton x)

    describe "fromSet" $
      prop "fromSet xs == fromList (Set.toList xs)" $ \(xs :: Set Integer) ->
        Counter.fromSet xs == Counter.fromList (Set.toList xs)

    describe "increment" $ do
      prop "singleton x <> singleton x == increment x (singleton x)" $ \(x :: String) ->
        Counter.singleton x <> Counter.singleton x == Counter.increment x (Counter.singleton x)
      prop "fromList [x, x] == increment x (singleton x)" $ \(x :: String) ->
        Counter.fromList [x, x] == Counter.increment x (Counter.singleton x)

    describe "lookup" $ do
      prop "lookup x (singleton x) == 1" $ \(x :: String) ->
        Counter.lookup x (Counter.singleton x) == 1
      prop "lookup x mempty == 0" $ \(x :: String) ->
        Counter.lookup x mempty == 0

    describe "valid" $ do
      prop "valid (singleton x)" $ \(x :: String) ->
        Counter.valid (Counter.singleton x)
      prop "valid (fromMap xs)" $ \(xs :: Map String Integer) ->
        Counter.valid (Counter.fromMap xs)
      prop "valid (fromCounts xs)" $ \(xs :: [(String, Integer)]) ->
        Counter.valid (Counter.fromCounts xs)
      prop "valid (fromList xs)" $ \(xs :: [String]) ->
        Counter.valid (Counter.fromList xs)
      prop "valid (fromSet xs)" $ \(xs :: Set String) ->
        Counter.valid (Counter.fromSet xs)
