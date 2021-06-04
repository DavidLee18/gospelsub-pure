module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (Json, JsonDecodeError, decodeJson)
import Data.Argonaut as Json
import Data.Array (catMaybes, elemIndex, head, (!!))
import Data.Either (Either(..), hush)
import Data.Generic.Rep (class Generic)
import Data.Gospel (class Listable, Gospel(..), Key(..), Verse, asArray, findVerse, fromString)
import Data.Gospel as Gospel
import Data.Maybe (Maybe(..), maybe')
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Firebase (collection, firestore, readCollection)
import Halogen (Component, HalogenM, SubscriptionId, defaultEval, gets, mkComponent, mkEval, modify_, subscribe', unsubscribe)
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
    , position :: Maybe (Gospel /\ (Either String Verse))
    , queue :: Array Gospel
    , route :: Route
    , subId :: Maybe SubscriptionId
    }

defaultState :: State
defaultState = { gospels: []
               , position: Nothing
               , queue: []
               , route: Home
               , subId: Nothing
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
mainComponent = mkComponent { initialState: \eithers -> defaultState { gospels = catMaybes $ hush <$> eithers }
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
    let rawKey = KE.key keyEvent
    let code = KE.code keyEvent
    Console.log $ "keyup code: " <> code <> ", key: " <> rawKey
    let key = fromString rawKey
    maybe' pure (flip handleKey sid) key
handleAction (Log json) = Console.logShow $ Json.toString json
handleAction (RouteTo Display) = do
    win <- liftEffect window
    subscribe' \sid -> eventListener keyup (toEventTarget win) (map (HandleEvent sid) <<< KE.fromEvent)
    modify_ \st -> st { position = head (_.queue st) >>= \g -> head $ asArray g >>= \c -> pure $ g /\ c, route = Display }
handleAction (RouteTo route) = do
    sid <- gets _.subId
    maybe' pure unsubscribe sid
    modify_ _ { route = route, subId = Nothing }

--verseOn :: Gospel -> Either String Verse -> (Int -> Int) -> Maybe (Either String Verse)
verseOn :: forall t13 t16. Listable t13 t16 => Eq t16 => t13 -> t16 -> (Int -> Int) -> Maybe t16
verseOn g v f = elemIndex v es >>= \i -> es !! (f i)
    where es = asArray g

handleKey :: forall slots output m. Gospel.Key -> SubscriptionId -> HalogenM State Action slots output m Unit
handleKey (Special Gospel.Esc) sid = do
        unsubscribe sid
        modify_ _ { route = Home, subId = Nothing }
handleKey (Key c) sid = do
    pos <- gets _.position
    let found = do
            g /\ v <- pos
            v' <- findVerse (Key c) g
            pure $ g /\ (pure v')
    modify_ _ { position = found <|> pos, subId = Just sid }
handleKey _ _ = pure unit