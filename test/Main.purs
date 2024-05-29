module Test.Main where

import Prelude

import Data.Gospel.SpecialKey (SpecialKey(..))
import Data.Maybe (Maybe(..))
import Data.String.Read (read)
import Effect (Effect)
import Test.QuickCheck (class Arbitrary, arbitrary, quickCheck, (<?>))

main :: Effect Unit
main = quickCheck \n ->
  let
    actual = read ("Alt " <> show (toInt n))
    expected = Just(AltKey (toInt n))
  in
    actual == expected <?> "n: " <> show n <> "\nActual: " <> show actual <> "\nExpected: " <> show expected

newtype OneDigit = OneDigit Int

derive newtype instance Show OneDigit

toInt :: OneDigit -> Int
toInt (OneDigit i) = i

instance Arbitrary OneDigit where
  arbitrary = OneDigit <<< (_ + 1) <<< flip mod 9 <<< absInt <$> arbitrary
   where
     absInt :: Int -> Int
     absInt i | i < 0 = -i
              | otherwise = i
