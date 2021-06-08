module Data.Gospel.Position where

import Prelude

import Data.Array (head)
import Data.Either (Either)
import Data.Gospel (Gospel)
import Data.Gospel.Verse (Verse)
import Data.Listable (asArray)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\), (/\))

newtype Position = Position (Maybe (Gospel /\ (Either String Verse)))

fromGospel :: Gospel -> Position
fromGospel g = Position do
    let es = asArray g
    v <- head es
    pure $ g /\ v