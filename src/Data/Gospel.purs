module Data.Gospel where

import Prelude

import Data.Array (drop, find, head, init, last, tail, take, takeEnd)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple.Nested (type (/\), (/\))

data Gospel
    = Gospel { name :: String, lyrics :: Array Verse }
    | Hymn { index :: Int, name :: String, lyrics :: Array Verse }

data SpecialKey
    = Home
    | End
    | PageUp
    | PageDown
    | Esc

derive instance eqSpecialKey :: Eq SpecialKey

data Key
    = Key Char
    | Special SpecialKey

derive instance eqKey :: Eq Key

data Verse
    = Verse String
    | TaggedVerse Key String

key :: Verse -> Maybe Key
key (Verse _) = Nothing
key (TaggedVerse k s) = Just k

keyVerse :: Key -> Verse -> Maybe String
keyVerse _ (Verse _)= Nothing
keyVerse k (TaggedVerse k' s) = if k == k' then Just s else Nothing

findVerse :: Key -> Gospel -> Maybe Verse
findVerse k (Gospel { lyrics }) = find (isJust <<< keyVerse k) lyrics
findVerse k (Hymn { lyrics }) = find (isJust <<< keyVerse k) lyrics

parseVerse :: String -> Verse
parseVerse s = case take 3 cs of
                ['[', c, ']'] -> TaggedVerse (Key c) (fromCharArray $ drop 3 cs)
                [n, '.', _] -> TaggedVerse (Key n) (fromCharArray cs)
                v -> Verse $ fromCharArray v
                where
                  cs = toCharArray s

--찬양 제목. 제목 (찬송가 번호) -> (제목, 찬송가 번호)
parseName :: String -> Maybe (String /\ (Maybe Int))
parseName s = if head cs == Just '-' && last cs == Just '-'
              then (/\) <$> (fromCharArray <$> title) <*> (parseIndex <<< takeEnd 5 <$> title)
              else Nothing
              where
                cs = toCharArray s
                title = Just cs >>= tail >>= init

foreign import parseIntImpl :: (forall x. x -> Maybe x) -> (forall x. Maybe x) -> String -> Maybe Int

parseInt :: String -> Maybe Int
parseInt = parseIntImpl Just Nothing

onlyNatural :: Int -> Maybe Int
onlyNatural i = if i >= 1 then Just i else Nothing

parseIndex :: Array Char -> Maybe Int
parseIndex ['(', a, b, c, ')'] = (parseInt <<< fromCharArray) [a, b, c] >>= onlyNatural
parseIndex [_, '(', a, b, ')'] = (parseInt <<< fromCharArray) [a, b] >>= onlyNatural
parseIndex [_, _, '(', a, ')'] = (parseInt <<< fromCharArray) [a] >>= onlyNatural
parseIndex _ = Nothing

mkVerses :: String -> Array Verse
mkVerses s = parseVerse <$> split (Pattern "\n\n") s