module Main where

import Prelude

import Data.Argonaut (JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Gospel (Gospel)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Effect.Firebase (collection, firestore, readCollection)
import React.Basic.Hooks as H

main :: Effect Unit
main = do
    fs <- firestore
    gospelsCol <- collection fs "gospels"
    launchAff_ do
        gospelsJson <- readCollection gospelsCol
        traverse log $ show <<< decodeJson <$> gospelsJson