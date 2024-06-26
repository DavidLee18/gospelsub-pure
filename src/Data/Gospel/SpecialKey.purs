module Data.Gospel.SpecialKey where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.String.Read (class Read, read)

data SpecialKey
    = Home
    | End
    | PageUp
    | PageDown
    | Esc
    | AltKey Int
    | UpArrow
    | DownArrow
    | LeftArrow
    | RightArrow

derive instance Eq SpecialKey
derive instance Ord SpecialKey
derive instance Generic SpecialKey _
instance Show SpecialKey where show = genericShow
instance Read SpecialKey
    where
        read "Home" = Just Home
        read "End" = Just End
        read "PageUp" = Just PageUp
        read "PageDown" = Just PageDown
        read "Escape" = Just Esc
        read "ArrowUp" = Just UpArrow
        read "ArrowDown" = Just DownArrow
        read "ArrowLeft" = Just LeftArrow
        read "ArrowRight" = Just RightArrow
        read s = case cs of
            ['A', 'l', 't', ' ', c] -> map AltKey $ onlyNatural =<< Int.fromString (singleton c)
            _ -> Nothing
            where cs = toCharArray s

instance EncodeJson SpecialKey where encodeJson = encodeJson <<< show
instance DecodeJson SpecialKey where
    decodeJson json = do
        x <- decodeJson json
        note (UnexpectedValue json) $ read x

onlyNatural :: Int -> Maybe Int
onlyNatural i = if i >= 1 then Just i else Nothing