module Data.Gospel where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson, (.:), (.:?))
import Data.Array (drop, find, take)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust)
import Data.Show.Generic (genericShow)
import Data.String (length)
import Data.String.CodeUnits (fromCharArray, toChar, toCharArray)

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

data SpecialKey
    = Home
    | End
    | PageUp
    | PageDown
    | Esc

derive instance eqSpecialKey :: Eq SpecialKey
derive instance genericSpecialKey :: Generic SpecialKey _
instance showSpecialKey :: Show SpecialKey where show = genericShow

instance encodeJsonSpecialKey :: EncodeJson SpecialKey where encodeJson = encodeJson <<< show
instance decodeJsonSpecialKey :: DecodeJson SpecialKey where
    decodeJson json = do
        x <- decodeJson json
        case x of
            "Home" -> Right Home
            "End" -> Right End
            "Page Up" -> Right PageUp
            "Page Down" -> Right PageDown
            "Esc" -> Right Esc
            _ -> Left (UnexpectedValue json)

data Key
    = Key Char
    | Special SpecialKey

derive instance eqKey :: Eq Key
derive instance genericKey :: Generic Key _
instance showKey :: Show Key where show = genericShow

instance encodeJsonKey :: EncodeJson Key where
    encodeJson (Key c) = encodeJson c
    encodeJson (Special s) = encodeJson s

instance decodeJsonKey :: DecodeJson Key where
    decodeJson json = do
        s <- decodeJson json
        case length s of
            1 -> note (UnexpectedValue json) $ Key <$> toChar s
            _ -> do
                s' <- decodeJson <<< encodeJson $ s
                pure $ Special s'


data Verse
    = Verse String
    | NumberedVerse Int String
    | TaggedVerse Key String

derive instance eqVerse :: Eq Verse
derive instance genericVerse :: Generic Verse _
instance showVerse :: Show Verse where show = genericShow

instance encodeJsonVerse :: EncodeJson Verse where
    encodeJson (Verse s) = encodeJson s
    encodeJson (TaggedVerse (Key c) s) = encodeJson $ fromCharArray ['[', c, ']'] <> s
    encodeJson (TaggedVerse (Special _) s) = encodeJson s
    encodeJson (NumberedVerse n s) = encodeJson $ show n <> ". " <> s

instance decodeJsonVerse :: DecodeJson Verse where
    decodeJson json = do
        s <- decodeJson json
        let cs = toCharArray s
        case take 3 cs of
            ['[', c, ']'] -> pure $ TaggedVerse (Key c) (fromCharArray $ drop 3 cs)
            [n, '.', ' '] -> do
                n' <- (decodeJson <<< encodeJson) $ n
                pure $ NumberedVerse n' s
            v -> pure $ Verse $ fromCharArray v


key :: Verse -> Maybe Key
key (Verse _) = Nothing
key (TaggedVerse k _) = Just k
key (NumberedVerse n _) = case toChar $ show n of
                            Just c -> Just $ Key c
                            Nothing -> Nothing

keyVerse :: Key -> Verse -> Maybe String
keyVerse _ (Verse _)= Nothing
keyVerse k (TaggedVerse k' s) = if k == k' then Just s else Nothing
keyVerse (Key k) (NumberedVerse n s) = if show k == show n then Just s else Nothing
keyVerse (Special _) (NumberedVerse _ _) = Nothing

findVerse :: Key -> Gospel -> Maybe Verse
findVerse k (Gospel { lyrics }) = find (isJust <<< keyVerse k) lyrics
findVerse k (Hymn { lyrics }) = find (isJust <<< keyVerse k) lyrics

foreign import parseIntImpl :: (forall x. x -> Maybe x) -> (forall x. Maybe x) -> String -> Maybe Int

parseInt :: String -> Maybe Int
parseInt = parseIntImpl Just Nothing

onlyNatural :: Int -> Maybe Int
onlyNatural i = if i >= 1 then Just i else Nothing