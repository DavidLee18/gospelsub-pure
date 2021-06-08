module Data.Listable where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes, head, uncons, (:))
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Gospel (Gospel(..), showTitle)
import Data.Gospel.Verse (Verse)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, stripPrefix, stripSuffix)

class Listable l i where
  asArray :: l -> Array i
  fromArray :: Array i -> Maybe l

instance listableGospelEitherStringVerse :: Listable Gospel (Either String Verse)
    where
        asArray :: Gospel -> Array (Either String Verse)
        asArray g@(Gospel { lyrics }) = Left (showTitle g) : (Right <$> lyrics)
        asArray g@(Hymn { lyrics }) = Left (showTitle g) : (Right <$> lyrics)

        fromArray :: Array (Either String Verse) -> Maybe Gospel
        fromArray a = do
            { head: mTitle, tail: eLyrics } <- uncons a
            case mTitle of
                Right _ -> Nothing
                Left title ->
                    let 
                        gospel = do
                            n <- stripSuffix (Pattern " -") =<< stripPrefix (Pattern "- ") title
                            pure $ Gospel { name: n, lyrics: catMaybes $ hush <$> eLyrics }
                        hymn = do
                            { head: index', tail: name' } <- uncons $ split (Pattern ". ") title
                            i <- Int.fromString index'
                            name <- if Array.length name' == 1 then head name' else Nothing
                            pure $ Hymn { name, index: i, lyrics: catMaybes $ hush <$> eLyrics }
                    in gospel <|> hymn