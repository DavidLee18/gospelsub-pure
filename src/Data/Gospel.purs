module Data.Gospel where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.:), (.:?))
import Data.Array (find)
import Data.Generic.Rep (class Generic)
import Data.Gospel.Key (Key)
import Data.Gospel.Verse (Verse, keyVerse)
import Data.Maybe (Maybe(..), isJust)
import Data.Show.Generic (genericShow)

data Gospel
    = Gospel { name :: String, lyrics :: Array Verse }
    | Hymn { index :: Int, name :: String, lyrics :: Array Verse }

derive instance eqGospel :: Eq Gospel
derive instance genericGospel :: Generic Gospel _
instance showGospel :: Show Gospel where show = genericShow

instance encodeJsonGospel :: EncodeJson Gospel where
    encodeJson (Gospel g) = encodeJson g
    encodeJson (Hymn h) = encodeJson h

instance decodeJsonGospel :: DecodeJson Gospel where
    decodeJson json = do
        x <- decodeJson json
        name <- x .: "name"
        lyrics <- x .: "lyrics"
        index <- x .:? "index"
        pure $ case index of
            Just i -> Hymn { index: i, name, lyrics }
            Nothing -> Gospel { name, lyrics }

findVerse :: Key -> Gospel -> Maybe Verse
findVerse k (Gospel { lyrics }) = find (isJust <<< keyVerse k) lyrics
findVerse k (Hymn { lyrics }) = find (isJust <<< keyVerse k) lyrics

showTitle :: Gospel -> String
showTitle (Hymn { index, name }) = show index <> ". " <> name
showTitle (Gospel { name }) = "- " <> name <> " -"