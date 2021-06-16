module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (extract)
import Data.Argonaut (Json, JsonDecodeError, decodeJson)
import Data.Argonaut as Json
import Data.Array (catMaybes, delete, elemIndex, head, intersperse, last, null, snoc, (!!))
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Generic.Rep (class Generic)
import Data.Gospel (Gospel, findVerse, showTitle)
import Data.Gospel.Key (Key(..))
import Data.Gospel.Key as Key
import Data.Gospel.SpecialKey as SpecialKey
import Data.Gospel.Verse (Verse)
import Data.Gospel.Verse as Verse
import Data.Listable (class Listable, asArray)
import Data.Maybe (Maybe(..), fromMaybe, maybe')
import Data.Show.Generic (genericShow)
import Data.String.Read (read)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Firebase (collection, firestore, readCollection)
import Halogen (Component, HalogenM, SubscriptionId, defaultEval, gets, mkComponent, mkEval, modify_, subscribe', unsubscribe)
import Halogen.Aff (awaitBody)
import Halogen.HTML (HTML, li)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (draggable, style)
import Halogen.HTML.Properties.ARIA (role)
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import Material.Elements (IconName(..), SelectedDetail, activatable, disabled, divider, hasMeta, icon, label, metaSlot, mwc_button, mwc_icon_button, mwc_list, mwc_list_item, onSelected, raised)
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
    { gospels :: Array Gospel
    , movingGospel :: Maybe Gospel
    , position :: Maybe (Gospel /\ (Either String Verse))
    , queue :: Array Gospel
    , route :: Route
    , subId :: Maybe SubscriptionId
    , textSize :: Int
    }

defaultState :: State
defaultState = { gospels: []
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
        runUI mainComponent gospels body

mainComponent :: forall query output m. MonadEffect m => Component query (Array (Either JsonDecodeError Gospel)) output m
mainComponent = mkComponent { initialState: \eithers -> defaultState { gospels = catMaybes $ hush <$> eithers }
                            , render
                            , eval: mkEval $ defaultEval { handleAction = handleAction }
                            }

render :: forall w. State -> HTML w Action
render { gospels, queue, route: Home } = HH.div_ [ HH.text "Hello Halogen!"
                                                 , mwc_list [] $ intersperse (li [ divider, role "seperator" ] []) $ map gospelView gospels
                                                 , mwc_list [ activatable, onSelected HandleSelectedEvent ] $ intersperse (li [ divider, role "seperator" ] []) $ map queueItemView queue
                                                 , mwc_button [ label "move up in queue", icon $ IconName "arrow_upward", onClick $ const QueueUp ]
                                                 , mwc_button [ label "move down in queue", icon $ IconName "arrow_downward", onClick $ const QueueDown ]
                                                 , mwc_button $ [ raised, label "Display", onClick $ const $ RouteTo Display ] <> if null queue then [ disabled ] else []
                                                 ]
render { position, route: Display, textSize } = HH.div_ [ HH.h2 [ style $ "font-size: " <> show textSize <> "px;" ] [ HH.text $ fromMaybe "Loading Gospels..." $ map Verse.string =<< hush <<< extract <$> position ] ]

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
    modify_ \st -> st { position = head (_.queue st) >>= \g -> head $ asArray g >>= \t -> pure $ g /\ t, route = Display }
handleAction (RouteTo route) = do
    sid <- gets _.subId
    maybe' pure unsubscribe sid
    modify_ _ { route = route, subId = Nothing }
handleAction (AddtoQueue g) = modify_ \st -> st { queue = _.queue st `snoc` g }
handleAction (RemovefromQueue g) = modify_ \st -> st { queue = delete g $ _.queue st }
handleAction QueueUp = do
    q <- gets _.queue
    mg <- gets _.movingGospel
    let newQueue = do
            g <- mg
            i <- elemIndex g q
            let { before, after } = Array.splitAt i q
            let newBefore = Array.dropEnd 1 before <> [ g ] <> Array.takeEnd 1 before
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
            let newAfter = Array.take 1 (Array.drop 1 after) <> [ g ] <> Array.drop 2 after
            pure $ before <> newAfter
    modify_ _ { queue = fromMaybe q newQueue }
handleAction (HandleSelectedEvent { index: indicies }) = do
    q <- gets _.queue
    let newGospel = Array.head indicies >>= \i -> q !! i
    modify_ \st -> st { movingGospel = newGospel <|> (_.movingGospel st) }

verseOnIndex :: (Int -> Int) -> Gospel -> Either String Verse -> Maybe (Either String Verse)
--verseOnIndex :: forall t13 t16. Listable t13 t16 => Eq t16 => (Int -> Int) -> t13 -> t16 -> Maybe t16
verseOnIndex f g v = elemIndex v es >>= \i -> es !! (f i)
    where es = asArray g

verseOnArray :: (Int -> Array (Either String Verse) -> Maybe (Either String Verse)) -> Gospel -> Either String Verse -> Maybe (Either String Verse)
--verseOnArray :: forall t21 t24 t25. Listable t21 t25 => Eq t25 => (Int -> Array t25 -> Maybe t24) -> t21 -> t25 -> Maybe t24
verseOnArray f g v = elemIndex v es >>= \i -> f i es
    where es = asArray g
--(a -> b -> m b) -> m (t a b) -> m (t a b)
mapPosition :: forall m. Monad m => (Gospel -> Either String Verse -> m (Either String Verse)) -> m (Gospel /\ (Either String Verse)) -> m (Gospel /\ (Either String Verse))
--mapPosition :: forall m t24 t34 t35. Monad m => (t34 -> t24 -> m t35) -> m (t34 /\ t24) -> m (t34 /\ t35)
mapPosition f p = p >>= \(g /\ v) -> f g v >>= \v' -> pure $ g /\ v'

--gospelOnIndex :: (Int -> Int) -> Gospel -> Array Gospel -> Maybe Gospel
gospelOnIndex :: forall t404. Eq t404 => (Int -> Int) -> t404 -> Array t404 -> Maybe t404
gospelOnIndex f g gs = elemIndex g gs >>= \i -> gs !! (f i)

handleKey :: forall slots output m. Key.Key -> SubscriptionId -> HalogenM State Action slots output m Unit
handleKey (Key c) _ = do
    pos <- gets _.position
    modify_ _ { position = mapPosition (\g _ -> Right <$> findVerse (Key c) g) pos <|> pos }
handleKey (Special SpecialKey.Esc) sid = do
    unsubscribe sid
    modify_ _ { route = Home, subId = Nothing }
handleKey (Special SpecialKey.UpArrow) _ = modify_ \st -> st { textSize = _.textSize st + 5 }
handleKey (Special SpecialKey.DownArrow) _ = do
    size <- gets _.textSize
    modify_ _ { textSize = if size - 5 > 0 then size - 5 else size }
handleKey (Special SpecialKey.LeftArrow) _ = do
    pos <- gets _.position
    modify_ _ { position = mapPosition (verseOnIndex (_ - 1)) pos <|> pos }
handleKey (Special SpecialKey.RightArrow) _ = do
    pos <- gets _.position
    modify_ _ { position = mapPosition (verseOnIndex (_ + 1)) pos <|> pos }
handleKey (Special SpecialKey.Home) _ = do
    pos <- gets _.position
    modify_ _ { position = mapPosition (verseOnIndex \_ -> 0) pos <|> pos }
handleKey (Special SpecialKey.End) _ = do
    pos <- gets _.position
    modify_ _ { position = mapPosition (verseOnArray $ const last) pos <|> pos }
handleKey (Special SpecialKey.PageUp) _ = do
    q <- gets _.queue
    pos <- gets _.position
    let newPos = pos >>= \(g /\ v) -> gospelOnIndex (_ - 1) g q >>= \g' -> head $ asArray g' >>= \t' -> pure $ g' /\ t'
    modify_ _ { position = newPos <|> pos }
handleKey (Special SpecialKey.PageDown) _ = do
    q <- gets _.queue
    pos <- gets _.position
    let newPos = pos >>= \(g /\ v) -> gospelOnIndex (_ + 1) g q >>= \g' -> head $ asArray g' >>= \t' -> pure $ g' /\ t'
    modify_ _ { position = newPos <|> pos }
handleKey (Special (SpecialKey.AltKey n)) _ = do
    q <- gets _.queue
    pos <- gets _.position
    let newPos = pos >>= \(g /\ v) -> gospelOnIndex (\_ -> n) g q >>= \g' -> head $ asArray g' >>= \t' -> pure $ g' /\ t'
    modify_ _ { position = newPos <|> pos }