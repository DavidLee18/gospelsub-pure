module Main where

import Prelude

import Data.Argonaut (JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Array (catMaybes)
import Data.Either (Either, either, hush)
import Data.Gospel (Gospel(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log, logShow)
import Effect.Firebase (collection, firestore, readCollection)
import Halogen (Component, HalogenM, defaultEval, mkComponent, mkEval, modify_)
import Halogen.Aff (awaitBody)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.VDom.Driver (runUI)
import Material.Elements (label, left, multi, mwc_button, mwc_check_list_item, mwc_list, onSelected, outlined, toSelectedDetail)
import Web.Event.Event (Event)

data Route
    = Home
    | Display

type State =
    { gospels :: Array Gospel
    , route :: Route
    , selectedIndicies :: Array Int
    }

data Action
    = Log Event
    | RouteTo Route

main :: Effect Unit
main = do
    fs <- firestore
    gospelsCol <- fs `collection` "gospels"
    launchAff_ do
        gospelsJson <- readCollection gospelsCol
        let gospels = decodeJson <$> gospelsJson :: Array (Either JsonDecodeError Gospel)
        traverse_ log $ either printJsonDecodeError show <$> gospels
        body <- awaitBody
        runUI mainComponent gospels body

mainComponent :: forall q o m. MonadEffect m => Component q (Array (Either JsonDecodeError Gospel)) o m
mainComponent = mkComponent { initialState: \eithers -> { gospels: catMaybes $ hush <$> eithers , route: Home, selectedIndicies: [] }
                            , render
                            , eval: mkEval $ defaultEval { handleAction = handleAction }
                            }

render :: forall w. State -> HTML w Action
render state = HH.div_ [ HH.text "Hello Halogen!"
                       , mwc_list [ multi, onSelected Log ] $ gospelView <$> state.gospels
                       , mwc_button [ outlined, label "Display", onClick $ \_ -> RouteTo Display ]
                       ]

gospelView :: forall w i. Gospel -> HTML w i
gospelView (Hymn { index, name, lyrics }) = mwc_check_list_item [ left ] [ HH.text $ show index <> ". " <> name ]
gospelView (Gospel { name, lyrics }) = mwc_check_list_item [ left ] [ HH.text name ]

handleAction :: forall output m. MonadEffect m => Action -> HalogenM State Action () output m Unit
handleAction (Log event) = do
    let detail = toSelectedDetail event
    logShow detail
    modify_ _ { selectedIndicies = detail.index }
handleAction (RouteTo route) = modify_ _ { route = route }