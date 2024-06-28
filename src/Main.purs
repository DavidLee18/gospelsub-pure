module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (extract)
import Control.Comonad.Env (Env, env, runEnv, withEnv)
import Data.Argonaut (Json, JsonDecodeError, decodeJson)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Either (Either, hush)
import Data.Generic.Rep (class Generic)
import Data.Gospel (Gospel(..), showTitle)
import Data.Gospel.Key (Key(..))
import Data.Gospel.Key as Key
import Data.Gospel.SpecialKey as SpecialKey
import Data.Gospel.Verse (Verse)
import Data.Gospel.Verse as Verse
import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.List.Lazy as List
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe')
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Read (read)
import Data.Tuple.Nested ((/\))
import Data.Zipper (Zipper(..), left, right, shiftLeft, shiftRight)
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
import Halogen.HTML.Properties (style)
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import Material.Elements (IconName(..), disabled, hasIcon, md_divider, md_filled_button, md_icon, md_list, md_list_item, md_outlined_button, md_outlined_icon_button)
import Type.Prelude (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, altKey)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes (keyup)

data Route
    = Home
    | Display

derive instance Generic Route _
instance Show Route where show = genericShow

type State =
    { blind :: Boolean
    , gospels :: Array Gospel
    , position :: Maybe (Zipper Verse)
    , queue :: Env Int (Array Gospel)
    , route :: Route
    , subId :: Maybe SubscriptionId
    , textSize :: Int
    }

defaultState :: State
defaultState = { blind: false
               , gospels: []
               , position: Nothing
               , queue: env 0 []
               , route: Home
               , subId: Nothing
               , textSize: 70
               }

data Action
    = AddtoQueue Gospel
    | HandleKeyEvent SubscriptionId KeyboardEvent
    | Log Json
    | QueueDown Gospel
    | QueueUp Gospel
    | RemovefromQueue Gospel
    | RouteTo Route

main :: Effect Unit
main = do
    fs <- firestore
    gospelsCol <- collection "gospels" fs
    launchAff_ do
        gospelsJson <- readCollection gospelsCol
        let gospels = decodeJson <$> gospelsJson :: Array (Either JsonDecodeError Gospel)
        body <- awaitBody
        void $ runUI mainComponent gospels body

mainComponent :: forall query output m. MonadEffect m => Component query (Array (Either JsonDecodeError Gospel)) output m
mainComponent = mkComponent { initialState: \eithers -> defaultState { gospels = Array.sort $ Array.catMaybes $ hush <$> eithers }
                            , render
                            , eval: mkEval $ defaultEval { handleAction = handleAction }
                            }

render :: forall w. State -> HTML w Action
render { gospels, queue, route: Home } = HH.div_ [ HH.h3_ [ HH.text "GospelSub in Halogen" ]
                                                 , HH.span_ [ HH.text "Gospels" ]
                                                 , md_list [] $ Array.intersperse (md_divider []) $ map gospelView gospels
                                                 , HH.span_ [ HH.text "Queue" ]
                                                 , md_list [] $ Array.intersperse (md_divider []) $ map queueItemView $ extract queue
                                                 , md_filled_button ([ onClick $ \_ -> RouteTo Display ] <> if Array.null $ extract queue then [ disabled ] else []) [ HH.text "Display", md_icon (IconName "slideshow") ]
                                                 ]
render { blind, position, route: Display, textSize } = HH.div_ [ HH.h2 [ style $ "font-size: " <> show textSize <> "px;" ] if blind then [] else fromMaybe [ HH.text "Loading Gospels..." ] titleOrVerse ]
    where titleOrVerse = Array.intersperse HH.br_ <<< map HH.text <<< String.split (Pattern "\\n") <<< Verse.string <<< extract <$> position

gospelView :: forall w. Gospel -> HTML w Action
gospelView g = md_list_item [] [ HH.span_ [ HH.text $ showTitle g ]
                               , md_outlined_icon_button [ onClick $ \_ -> AddtoQueue g ] [ md_icon (IconName "add_circle_outline") ]
                               ]

queueItemView :: forall w. Gospel -> HTML w Action
queueItemView g = md_list_item [] [ HH.span_ [ HH.text $ showTitle g ]
                                  , md_outlined_button [ hasIcon, onClick $ \_ -> QueueUp g ] [ HH.text "move up in queue", md_icon (IconName "arrow_upward") ]
                                  , md_outlined_button [ hasIcon, onClick $ \_ -> QueueDown g ] [ HH.text "move down in queue", md_icon (IconName "arrow_downward") ]
                                  , md_outlined_icon_button [ onClick $ \_ -> RemovefromQueue g ] [ md_icon (IconName "remove_circle_outline") ]
                                  ]

handleAction :: forall slots output m . MonadEffect m => Action -> HalogenM State Action slots output m Unit
handleAction (HandleKeyEvent sid keyEvent) = do
    liftEffect $ preventDefault $ KE.toEvent keyEvent
    let rawKey = KE.key keyEvent
    let code = KE.code keyEvent
    Console.log $ "keyup code: " <> code <> ", key: " <> rawKey
    let key = if altKey keyEvent then read $ "Alt " <> rawKey else read rawKey
    maybe' pure (flip handleKey sid) key
    modify_ _ { subId = Just sid }
handleAction (Log json) = Console.log $ Json.stringify json
handleAction (RouteTo Display) = do
    win <- liftEffect window
    subscribe' \sid -> eventListener keyup (toEventTarget win) (map (HandleKeyEvent sid) <<< KE.fromEvent)
    modify_ \st -> st { position = let i /\ qs = runEnv st.queue in positionFromGospel <$> qs Array.!! i, route = Display }
handleAction (RouteTo route) = do
    sid <- gets _.subId
    maybe' pure unsubscribe sid
    modify_ _ { route = route, subId = Nothing }
handleAction (AddtoQueue g) = modify_ $ over (prop (Proxy :: Proxy "queue")) (map (insertUnique g))
handleAction (RemovefromQueue g) = modify_ $ over (prop (Proxy :: Proxy "queue")) (map (Array.delete g))
handleAction (QueueUp g) = do
    i /\ q <- runEnv <$> gets _.queue
    let newQueue = let
                        { before, after } = Array.splitAt i q
                        newBefore = Array.dropEnd 1 before <> [g] <> Array.takeEnd 1 before
                        newAfter = Array.drop 1 after
                in newBefore <> newAfter
    modify_ _ { queue = env (i-1) newQueue }
handleAction (QueueDown g) = do
    i /\ q <- runEnv <$> gets _.queue
    let newQueue = let
                    { before, after } = Array.splitAt i q
                    newAfter = Array.take 1 (Array.drop 1 after) <> [g] <> Array.drop 2 after
                in before <> newAfter
    modify_ _ { queue = env (i+1) newQueue }

insertUnique :: forall a. Eq a => a -> Array a -> Array a
insertUnique a as | Array.elem a as = as
                  | otherwise = a Array.: as

positionFromGospel :: Gospel -> Zipper Verse
positionFromGospel g@(Hymn { lyrics }) = Zipper List.nil (Verse.Verse $ showTitle g) (List.fromFoldable lyrics)
positionFromGospel g@(Gospel { lyrics }) = Zipper List.nil (Verse.Verse $ showTitle g) (List.fromFoldable lyrics)

gotoFirst :: forall a. Zipper a -> Zipper a
gotoFirst z@(Zipper ls _ _) | List.null ls = z
                            | otherwise = gotoFirst (right z)

gotoLast :: forall a. Zipper a -> Zipper a
gotoLast z@(Zipper _ _ rs) | List.null rs = z
                           | otherwise = gotoLast (left z)

findVerse :: Key.Key -> Zipper Verse -> Zipper Verse
findVerse k z@(Zipper ls a rs) | isJust $ Verse.keyVerse k a = z
                               | otherwise = case List.findIndex (isJust <<< Verse.keyVerse k) ls of
                                    Just i -> shiftRight i z
                                    Nothing -> case List.findIndex (isJust <<< Verse.keyVerse k) rs of
                                        Just j -> shiftLeft j z
                                        Nothing -> z

nth :: forall a. Int -> Zipper a -> Maybe a
nth i (Zipper ls a rs) | List.length ls == i = Just a
                       | otherwise = ls List.!! i <|> rs List.!! (i - List.length ls)

handleKey :: forall slots output m. Key.Key -> SubscriptionId -> HalogenM State Action slots output m Unit
handleKey (Key '0') _ = modify_ $ over (prop (Proxy :: Proxy "blind")) not
handleKey (Key '/') _ = modify_ $ over (prop (Proxy :: Proxy "position")) (map gotoFirst)
handleKey (Key '\\') _ = modify_ $ over (prop (Proxy :: Proxy "position")) (map gotoLast)
handleKey (Key ',') _ = do
    q <- gets _.queue
    let newQ = withEnv (\i -> min (Array.length $ extract q) (i+1)) q
    let i /\ qs = runEnv newQ
    let newPos = positionFromGospel <$> qs Array.!! i
    modify_ _ { position = newPos, queue = newQ }
handleKey (Key '.') _ = do
    q <- gets _.queue
    let newQ = withEnv (\i -> max 0 (i-1)) q
    let i /\ qs = runEnv newQ
    let newPos = positionFromGospel <$> qs Array.!! i
    modify_ _ { position = newPos, queue = newQ }
handleKey (Key c) _ = modify_ $ over (prop (Proxy :: Proxy "position")) (map (findVerse (Key c)))
handleKey (Special SpecialKey.Esc) sid = do
    unsubscribe sid
    modify_ _ { route = Home, subId = Nothing }
handleKey (Special SpecialKey.UpArrow) _ = modify_ $ over (prop (Proxy :: Proxy "textSize")) (_ + 5)
handleKey (Special SpecialKey.DownArrow) _ = modify_ $ over (prop (Proxy :: Proxy "textSize")) (\i -> max (i - 5) 5)
handleKey (Special SpecialKey.LeftArrow) _ = modify_ $ over (prop (Proxy :: Proxy "position")) (map right)
handleKey (Special SpecialKey.RightArrow) _ = modify_ $ over (prop (Proxy :: Proxy "position")) (map left)
handleKey (Special (SpecialKey.AltKey n)) _ = do
    q <- gets _.queue
    pos <- gets _.position
    let newPos = positionFromGospel <$> extract q Array.!! (n-1)
    modify_ _ { position = newPos <|> pos }
handleKey (Special _) _ = pure unit
