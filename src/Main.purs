module Main where

import Prelude

import Data.Argonaut (JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Array (catMaybes, singleton)
import Data.Either (Either, either, hush)
import Data.Gospel (Gospel(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Effect.Firebase (collection, firestore, readCollection)
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.Aff (awaitBody)
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
    fs <- firestore
    gospelsCol <- collection fs "gospels"
    launchAff_ do
        gospelsJson <- readCollection gospelsCol
        let gospels = decodeJson <$> gospelsJson :: Array (Either JsonDecodeError Gospel)
        traverse_ log $ either printJsonDecodeError show <$> gospels
        body <- awaitBody
        runUI mainComponent gospels body

mainComponent :: forall q o m. Component q (Array (Either JsonDecodeError Gospel)) o m
mainComponent = mkComponent { initialState: catMaybes <<< map hush
                            , render
                            , eval: mkEval defaultEval
                            }
                            where
                              render gospels = HH.div_ [ HH.text "Hello Halogen!"
                                                       , HH.ul_ $ gospelView <$> gospels
                                                       ]

                              gospelView (Hymn { index, name, lyrics }) = HH.li_ [ HH.text $ show index <> ". " <> name
                                                                                 , HH.ul_ $ HH.li_ <<< singleton <<< HH.text <<< show <$> lyrics
                                                                                 ]
                              gospelView (Gospel { name, lyrics }) = HH.li_ [ HH.text name
                                                                            , HH.ul_ $ HH.li_ <<< singleton <<< HH.text <<< show <$> lyrics
                                                                            ]