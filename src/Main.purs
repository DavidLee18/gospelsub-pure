module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (extract)
import Data.Argonaut (Json, JsonDecodeError, decodeJson)
import Data.Argonaut as Json
import Data.Array (catMaybes, delete, elemIndex, head, index, intersperse, last, null, sort, (!!))
import Data.Array as Array
import Data.Either (Either(..), either, hush)
import Data.Generic.Rep (class Generic)
import Data.Gospel (Gospel, lookUpVerse, showTitle)
import Data.Gospel.Key (Key(..))
import Data.Gospel.Key as Key
import Data.Gospel.SpecialKey as SpecialKey
import Data.Gospel.Verse (Verse)
import Data.Gospel.Verse as Verse
import Data.Lazy (Lazy)
import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.Listable (asArray)
import Data.Maybe (Maybe(..), fromMaybe, maybe')
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Read (read)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Firebase (collection, firestore, readCollection)
import Halogen (Component, HalogenM, SubscriptionId, defaultEval, defer, gets, mkComponent, mkEval, modify_, subscribe', unsubscribe)
import Halogen.Aff (awaitBody)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (draggable, style)
import Halogen.HTML.Properties.ARIA (role)
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import Material.Elements (IconName(..), SelectedDetail, activatable, disabled, divider, hasMeta, icon, label, metaSlot, mwc_button, mwc_icon_button, mwc_list, mwc_list_item, onSelected, raised)
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

derive instance genericRoute :: Generic Route _
instance showRoute :: Show Route where show = genericShow

type State =
    { blind :: Boolean
    , gospels :: Array Gospel
    , movingGospel :: Maybe Gospel
    , position :: Maybe (Gospel /\ (Either String Verse))
    , queue :: Array Gospel
    , route :: Route
    , subId :: Maybe SubscriptionId
    , textSize :: Int
    }

defaultState :: State
defaultState = { blind: false
               , gospels: []
               , movingGospel: Nothing
               , position: Nothing
               , queue: []
               , route: Home
               , subId: Nothing
               , textSize: 70
               }

data Action
    = AddtoQueue Gospel
    | HandleKeyEvent SubscriptionId KeyboardEvent
    | HandleSelectedEvent SelectedDetail
    | Log Json
    | QueueDown
    | QueueUp
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
mainComponent = mkComponent { initialState: \eithers -> defaultState { gospels = sort $ catMaybes $ hush <$> eithers }
                            , render
                            , eval: mkEval $ defaultEval { handleAction = handleAction }
                            }

render :: forall w. State -> HTML w Action
render { gospels, queue, route: Home } = HH.div_ [ HH.h3_ [ HH.text "GospelSub in Halogen" ]
                                                 , HH.span_ [ HH.text "Gospels" ]
                                                 , mwc_list [] $ intersperse (HH.li [ divider, role "seperator" ] []) $ map gospelView gospels
                                                 , HH.span_ [ HH.text "Queue" ]
                                                 , mwc_list [ activatable, onSelected HandleSelectedEvent ] $ intersperse (HH.li [ divider, role "seperator" ] []) $ map queueItemView queue
                                                 , mwc_button $ [ label "move up in queue", icon $ IconName "arrow_upward", onClick $ const QueueUp ] <> if null queue then [ disabled ] else []
                                                 , mwc_button $ [ label "move down in queue", icon $ IconName "arrow_downward", onClick $ const QueueDown ] <> if null queue then [ disabled ] else []
                                                 , mwc_button $ [ raised, label "Display", onClick $ const $ RouteTo Display ] <> if null queue then [ disabled ] else []
                                                 ]
render { blind, position, route: Display, textSize } = HH.div_ [ HH.h2 [ style $ "font-size: " <> show textSize <> "px;" ] if blind then [] else fromMaybe [ HH.text "Loading Gospels..." ] titleOrVerse ]
    where titleOrVerse = Array.intersperse HH.br_ <<< map HH.text <<< String.split (Pattern "\\n") <<< either identity Verse.string <<< extract <$> position

gospelView :: forall w. Gospel -> HTML w Action
gospelView g = mwc_list_item [ hasMeta ] [ HH.span_ [ HH.text $ showTitle g ] 
                                         , mwc_icon_button [ metaSlot, icon $ IconName "add_circle_outline", onClick $ const $ AddtoQueue g ]
                                         ]

queueItemView :: forall w. Gospel -> HTML w Action
queueItemView g = mwc_list_item [ hasMeta, draggable true ] [ HH.span_ [ HH.text $ showTitle g ]
                                                            , mwc_icon_button [ metaSlot, icon $ IconName "remove_circle_outline", onClick $ const $ RemovefromQueue g ]
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
handleAction (Log json) = Console.logShow $ Json.toString json
handleAction (RouteTo Display) = do
    win <- liftEffect window
    subscribe' \sid -> eventListener keyup (toEventTarget win) (map (HandleKeyEvent sid) <<< KE.fromEvent)
    modify_ \st -> st { position = positionFromGospel <$> head (_.queue st), route = Display }
handleAction (RouteTo route) = do
    sid <- gets _.subId
    maybe' pure unsubscribe sid
    modify_ _ { route = route, subId = Nothing }
handleAction (AddtoQueue g) = modify_ $ over (prop (Proxy :: Proxy "queue")) \q -> if Array.elem g q then q else q <> [g]
handleAction (RemovefromQueue g) = modify_ $ over (prop (Proxy :: Proxy "queue")) (delete g)
handleAction QueueUp = do
    q <- gets _.queue
    mg <- gets _.movingGospel
    let newQueue = do
            g <- mg
            i <- elemIndex g q
            let { before, after } = Array.splitAt i q
            let newBefore = Array.dropEnd 1 before <> [g] <> Array.takeEnd 1 before
            let newAfter = Array.drop 1 after
            pure $ newBefore <> newAfter
    modify_ _ { queue = fromMaybe q newQueue }
handleAction QueueDown = do
    q <- gets _.queue
    mg <- gets _.movingGospel
    let newQueue = do
            g <- mg
            i <- elemIndex g q
            let { before, after } = Array.splitAt i q
            let newAfter = Array.take 1 (Array.drop 1 after) <> [g] <> Array.drop 2 after
            pure $ before <> newAfter
    modify_ _ { queue = fromMaybe q newQueue }
handleAction (HandleSelectedEvent { index: indicies }) = do
    q <- gets _.queue
    let newGospel = Array.head indicies >>= index q
    modify_ \st -> st { movingGospel = newGospel <|> (_.movingGospel st) }

positionFromGospel :: forall t63. Gospel -> Gospel /\ (Either String t63)
positionFromGospel g = g /\ (Left $ showTitle g)

verseOnIndex :: (Int -> Int) -> Gospel /\ Either String Verse -> Maybe (Either String Verse)
verseOnIndex f (g /\ v) = do
    let es = asArray g
    i <- elemIndex v es
    es !! f i

verseOnArray :: (Int -> Array (Either String Verse) -> Maybe (Either String Verse)) -> Gospel /\ Either String Verse -> Maybe (Either String Verse)
verseOnArray f (g /\ v) = do
    let es = asArray g
    i <- elemIndex v es
    f i es

mapPosition :: forall m. Monad m => (Gospel /\ Either String Verse -> m (Either String Verse)) -> m (Gospel /\ (Either String Verse)) -> m (Gospel /\ (Either String Verse))
mapPosition f p = (/\) <$> (fst <$> p) <*> (f =<< p)

gospelOnIndex :: (Int -> Int) -> Gospel -> Array Gospel -> Maybe Gospel
gospelOnIndex f g gs = do
    i <- elemIndex g gs
    gs !! f i

handleKey :: forall slots output m. Key.Key -> SubscriptionId -> HalogenM State Action slots output m Unit
handleKey (Key '0') _ = modify_ $ over (prop (Proxy :: Proxy "blind")) not
handleKey (Key '/') _ = modify_ $ over (prop (Proxy :: Proxy "position")) \pos -> mapPosition (verseOnIndex \_ -> 0) pos <|> pos
handleKey (Key '\\') _ = modify_ $ over (prop (Proxy :: Proxy "position")) \pos -> mapPosition (verseOnArray $ const last) pos <|> pos
handleKey (Key ',') _ = do
    q <- gets _.queue
    pos <- gets _.position
    let newPos = pos >>= \(g /\ _) -> positionFromGospel <$> gospelOnIndex (_ - 1) g q
    modify_ _ { position = newPos <|> pos }
handleKey (Key '.') _ = do
    q <- gets _.queue
    pos <- gets _.position
    let newPos = pos >>= \(g /\ _) -> positionFromGospel <$> gospelOnIndex (_ + 1) g q
    modify_ _ { position = newPos <|> pos }
handleKey (Key c) _ = modify_ $ over (prop (Proxy :: Proxy "position")) \pos -> mapPosition (\(g /\ _) -> Right <$> lookUpVerse (Key c) g) pos <|> pos
handleKey (Special SpecialKey.Esc) sid = do
    unsubscribe sid
    modify_ _ { route = Home, subId = Nothing }
handleKey (Special SpecialKey.UpArrow) _ = modify_ $ over (prop (Proxy :: Proxy "textSize")) (_ + 5)
handleKey (Special SpecialKey.DownArrow) _ = modify_ $ over (prop (Proxy :: Proxy "textSize")) (\i -> max (i - 5) 5)
handleKey (Special SpecialKey.LeftArrow) _ = modify_ $ over (prop (Proxy :: Proxy "position")) \pos -> mapPosition (verseOnIndex (_ - 1)) pos <|> pos
handleKey (Special SpecialKey.RightArrow) _ = modify_ $ over (prop (Proxy :: Proxy "position")) \pos -> mapPosition (verseOnIndex (_ + 1)) pos <|> pos
handleKey (Special (SpecialKey.AltKey n)) _ = do
    q <- gets _.queue
    pos <- gets _.position
    let newPos = pos >>= \(g /\ _) -> positionFromGospel <$> gospelOnIndex (\_ -> n) g q
    modify_ _ { position = newPos <|> pos }
handleKey (Special _) _ = pure unit