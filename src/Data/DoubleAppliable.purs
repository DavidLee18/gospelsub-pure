module Data.DoubleAppliable where

import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

class DoubleAppliable p where
    doubleApply :: forall a b c. (a -> b -> c) -> p a b -> c

instance doubleAppliableTuple :: DoubleAppliable Tuple where
    doubleApply f (a /\ b) = f a b