module Data.NestedExtend where

import Prelude

import Control.Extend (class Extend)
import Data.Gospel (Gospel)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

class Extend w <= NestedExtend w m
    where nestedExtend :: forall a b. (w a -> m b) -> w a -> m (w b)

infixr 1 nestedExtend as <<>=

instance nestedExtendTupleMaybe :: NestedExtend (Tuple Gospel) Maybe
    where
        nestedExtend f a@(g /\ v) = case f a of
            Nothing -> Nothing
            Just v' -> Just $ g /\ v'