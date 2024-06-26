module Data.Gospel.Key where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Gospel.SpecialKey (SpecialKey)
import Data.Show.Generic (genericShow)
import Data.String (length)
import Data.String.CodeUnits (toChar)
import Data.String.Read (class Read, read)

data Key
    = Key Char
    | Special SpecialKey

derive instance Eq Key
derive instance Ord Key
derive instance Generic Key _
instance Show Key where show = genericShow
instance Read Key
    where read s = case length s of
            1 -> Key <$> toChar s
            _ -> Special <$> read s

instance EncodeJson Key where
    encodeJson (Key c) = encodeJson c
    encodeJson (Special s) = encodeJson s

instance DecodeJson Key where
    decodeJson json = do
        s <- decodeJson json
        note (UnexpectedValue json) $ read s