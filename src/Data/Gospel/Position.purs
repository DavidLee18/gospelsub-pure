module Data.Gospel.Position where

import Prelude

import Data.Either (Either(..))
import Data.Gospel (Gospel(..))
import Data.Gospel.Verse (Verse)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\), (/\))

newtype Position = Position (Maybe (Gospel /\ (Either String Verse)))

fromGospel :: Gospel -> Position
fromGospel g = Position $ pure $ g /\ (Left $ name g)
    where name (Hymn { name: n }) = n
          name (Gospel { name: n }) = n