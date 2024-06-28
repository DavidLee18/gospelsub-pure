module Data.Zipper where

import Prelude

import Control.Comonad (class Comonad, class Extend)
import Data.List.Lazy (List, Step(..), delete, elem, iterate, step, toUnfoldable, (:))
import Data.Maybe (Maybe(..))

data Zipper a = Zipper (List a) a (List a)

derive instance Functor Zipper

left :: forall a. Zipper a -> Zipper a
left z@(Zipper ls a rs) = case step rs of
    Nil -> z
    Cons a' rs' -> Zipper (a:ls) a' rs'

right :: forall a. Zipper a -> Zipper a
right z@(Zipper ls a rs) = case step ls of
    Nil -> z
    Cons a' ls' -> Zipper ls' a' (a:rs)

instance Extend Zipper where
    extend f = map f <<< dup
        where dup z = Zipper (iterate left z) z (iterate right z)

instance Comonad Zipper where extract (Zipper _ a _) = a


shiftLeft :: forall a. Int -> Zipper a -> Zipper a
shiftLeft i z | i <= 0 = z
              | otherwise = shiftLeft (i-1) (left z)

shiftRight :: forall a. Int -> Zipper a -> Zipper a
shiftRight i z | i <= 0 = z
               | otherwise = shiftLeft (i-1) (right z)


toArray :: forall a. Zipper a  -> Array a
toArray (Zipper ls a rs) = toUnfoldable ls <> [a] <> toUnfoldable rs

remove :: forall a. Eq a => a -> Zipper a -> Maybe (Zipper a)
remove a (Zipper ls a' rs) | a == a' = Nothing
                           | elem a ls = Just $ Zipper (delete a ls) a' rs
                           | elem a rs = Just $ Zipper ls a' (delete a rs)
                           | otherwise = Nothing
