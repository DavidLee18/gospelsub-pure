module Data.Gospel.Verse where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Array (take)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Gospel.Key (Key(..))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (drop)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Read (class Read, read)

data Verse
    = Verse String
   -- | NumberedVerse Int String
    | TaggedVerse Key String

derive instance Eq Verse
derive instance Ord Verse
derive instance Generic Verse _
instance Show Verse where show = genericShow
instance Read Verse
    where
        read s = case take 3 cs of
            [ '[', c, ']' ] -> pure $ TaggedVerse (Key c) (drop 3 s)
            [ n, '.', ' ' ] -> pure $ TaggedVerse (Key n) s
            _ -> pure $ Verse s
            where cs = toCharArray s

instance EncodeJson Verse where
    encodeJson (Verse s) = encodeJson s
    encodeJson (TaggedVerse (Key c) s) = encodeJson $ fromCharArray [ '[', c, ']' ] <> s
    encodeJson (TaggedVerse _ s) = encodeJson s

instance DecodeJson Verse where
    decodeJson json = do
        s <- decodeJson json
        note (UnexpectedValue json) $ read s
        

string :: Verse -> String
string (Verse s) = s
string (TaggedVerse _ s) = s

key :: Verse -> Maybe Key
key (TaggedVerse k _) = Just k
key _ = Nothing

keyVerse :: Key -> Verse -> Maybe String
keyVerse k (TaggedVerse k' s) | k == k' = Just s
keyVerse _ _ = Nothing