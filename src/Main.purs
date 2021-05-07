module Main where

import Prelude

import Data.Argonaut (JsonDecodeError, decodeJson, stringify)
import Data.Either (Either)
import Data.Gospel (Gospel)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Effect.Firebase (collection, firestore, readCollection)

main :: Effect Unit
main = do
    fs <- firestore
    gospelsCol <- collection fs "gospels"
    launchAff_ do
        gospelsJson <- readCollection gospelsCol
        let gospels = (decodeJson <$> gospelsJson) :: Array (Either JsonDecodeError Gospel)
        traverse log $ show <$> gospels