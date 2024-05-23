module Data.Gospel where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.:), (.:?))
import Data.Array.NonEmpty (NonEmptyArray, find)
import Data.Generic.Rep (class Generic)
import Data.Gospel.Key (Key)
import Data.Gospel.Verse (Verse, keyVerse)
import Data.Lens (Lens', lens')
import Data.Maybe (Maybe(..), isJust)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))

data Gospel
    = Gospel { name :: String, lyrics :: NonEmptyArray Verse }
    | Hymn { index :: Int, name :: String, lyrics :: NonEmptyArray Verse }

derive instance Eq Gospel
derive instance Ord Gospel
derive instance Generic Gospel _
instance Show Gospel where show = genericShow

instance EncodeJson Gospel where
    encodeJson (Gospel g) = encodeJson g
    encodeJson (Hymn h) = encodeJson h

instance DecodeJson Gospel where
    decodeJson json = do
        x <- decodeJson json
        name <- x .: "name"
        lyrics <- x .: "lyrics"
        index <- x .:? "index"
        pure $ case index of
            Just i -> Hymn { index: i, name, lyrics }
            Nothing -> Gospel { name, lyrics }

lookUpVerse :: Key -> Gospel -> Maybe Verse
lookUpVerse k (Gospel { lyrics }) = find (isJust <<< keyVerse k) lyrics
lookUpVerse k (Hymn { lyrics }) = find (isJust <<< keyVerse k) lyrics

showTitle :: Gospel -> String
showTitle (Hymn { index, name }) = show index <> ". " <> name
showTitle (Gospel { name }) = "- " <> name <> " -"

_name :: Lens' Gospel String
_name = lens' \g -> case g of
    Hymn h@{ name: n } -> n /\ \n' -> Hymn (h { name = n' })
    Gospel g'@{ name: n } -> n /\ \n' -> Gospel (g' { name = n' })