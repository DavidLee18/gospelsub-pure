module Control.NestedExtend where

import Prelude

import Control.Comonad (class Comonad)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))

class (Comonad w, Monad m) <= NestedExtend w m where
  nestedExtend :: forall a b. (w a -> m b) -> w a -> m (w b)

infixr 1 nestedExtend as <<>=

instance nestedExtendTupleMaybe :: NestedExtend (Tuple g) Maybe
    where
        nestedExtend :: forall a b c. (c /\ a -> Maybe b) -> c /\ a -> Maybe (c /\ b)
        nestedExtend f a@(g /\ v) = f a >>= \v' -> pure $ g /\ v'