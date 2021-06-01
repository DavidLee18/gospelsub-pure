module Main where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, decodeJson, toString)
import Data.Array (catMaybes)
import Data.Either (Either, hush)
import Data.Generic.Rep (class Generic)
import Data.Gospel (Gospel(..))
import Data.Maybe (Maybe(..), maybe')
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Firebase (collection, firestore, readCollection)
import Halogen (Component, HalogenM, SubscriptionId, defaultEval, get, mkComponent, mkEval, modify_, subscribe', unsubscribe)
import Halogen.Aff (awaitBody)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import Material.Elements (IconName(..), hasMeta, icon, label, metaSlot, multi, mwc_button, mwc_icon_button, mwc_list, mwc_list_item, raised)
import Web.Event.Event (preventDefault)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes (keyup)

data Route
    = Home
    | Display

derive instance genericRoute :: Generic Route _
instance showRoute :: Show Route where show = genericShow

type State =
    { gospels :: Array Gospel
    , route :: Route
    , subId :: Maybe SubscriptionId
    }

data Action
    = HandleEvent SubscriptionId KeyboardEvent
    | Log Json
    | RouteTo Route

main :: Effect Unit
main = do
    fs <- firestore
    gospelsCol <- collection "gospels" fs
    launchAff_ do
        gospelsJson <- readCollection gospelsCol
        let gospels = decodeJson <$> gospelsJson :: Array (Either JsonDecodeError Gospel)
        body <- awaitBody
        runUI mainComponent gospels body

mainComponent :: forall query output m. MonadEffect m => Component query (Array (Either JsonDecodeError Gospel)) output m
mainComponent = mkComponent { initialState: \eithers -> { gospels: catMaybes $ hush <$> eithers, route: Home, subId: Nothing }
                            , render
                            , eval: mkEval $ defaultEval { handleAction = handleAction }
                            }

render :: forall w. State -> HTML w Action
render { gospels, route: Home } = HH.div_ [ HH.text "Hello Halogen!"
                                          , mwc_list [ multi ] $ gospelView <$> gospels
                                          , mwc_button [ raised, label "Display", onClick $ \_ -> RouteTo Display ]
                                          ]
render { route: Display } = HH.div_ [ HH.h2_ [ HH.text "Display Page!" ] ]

gospelView :: forall w i. Gospel -> HTML w i
gospelView (Hymn { index, name, lyrics }) = mwc_list_item [ hasMeta ] [ HH.span_ [ HH.text $ show index <> ". " <> name ]
                                                                      , mwc_icon_button [ metaSlot, icon $ IconName "add_circle_outline" ]
                                                                      ]
gospelView (Gospel { name, lyrics }) = mwc_list_item [ hasMeta ] [ HH.span_ [ HH.text name ]
                                                                 , mwc_icon_button [ metaSlot, icon (IconName "add_circle_outline") ]
                                                                 ]

handleAction :: forall slots output m . MonadEffect m => Action -> HalogenM State Action slots output m Unit
handleAction (HandleEvent sid keyEvent) = do
    liftEffect $ preventDefault $ KE.toEvent keyEvent
    let key = KE.key keyEvent
    let code = KE.code keyEvent
    log $ "keyup code: " <> code <> ", key: " <> key
    if key == "Escape" then do
        unsubscribe sid
        modify_ _ { route = Home, subId = Nothing }
    else modify_ _ { subId = Just sid }
handleAction (Log json) = logShow $ toString json
handleAction (RouteTo Display) = do
    win <- liftEffect window
    subscribe' \sid -> eventListener keyup (toEventTarget win) ((<$>) (HandleEvent sid) <<< KE.fromEvent)
    modify_ _ { route = Display }
handleAction (RouteTo route) = do
    state <- get
    let sid = _.subId state
    maybe' pure unsubscribe sid
    modify_ _ { route = route, subId = Nothing }