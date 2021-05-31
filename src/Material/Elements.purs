module Material.Elements where

import Data.Maybe (Maybe(..))
import Halogen (ElemName(..), PropName(..))
import Halogen.HTML.Core (class IsProp, toPropValue)
import Halogen.HTML.Elements (Leaf, Node)
import Halogen.HTML.Elements as HE
import Halogen.HTML.Events (handler)
import Halogen.HTML.Properties (IProp, prop)
import Web.Event.Event (EventType(..))
import Web.Event.Internal.Types (Event)

type IndexDiff = { added :: Array Int, removed :: Array Int }

type SelectedDetail = { index :: Array Int, diff :: Maybe IndexDiff }

foreign import toSelectedDetailImpl :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Event -> SelectedDetail

toSelectedDetail :: Event -> SelectedDetail
toSelectedDetail = toSelectedDetailImpl Just Nothing

mwc_button :: forall r w i. Leaf r w i
mwc_button props = HE.element (ElemName "mwc-button") props []

outlined :: forall r i. IProp r i
outlined = prop (PropName "outlined") true

raised :: forall r i. IProp r i
raised = prop (PropName "raised") true

label :: forall r i. String -> IProp r i
label = prop (PropName "label")

newtype IconName = IconName String

instance isPropIconName :: IsProp IconName where
    toPropValue (IconName i) = toPropValue i

icon :: forall r i. IconName -> IProp r i
icon = prop (PropName "icon")

mwc_circular_progress :: forall r w i. Leaf r w i
mwc_circular_progress props = HE.element (ElemName "mwc-circular-progress") props []

indeterminate :: forall r i. IProp r i
indeterminate = prop (PropName "indeterminate") true

mwc_icon_button :: forall r w i. Leaf r w i
mwc_icon_button props = HE.element (ElemName "mwc-icon-button") props []

disabled :: forall r i. IProp r i
disabled = prop (PropName "disabled") true

mwc_list :: forall r w i. Node r w i
mwc_list = HE.element (ElemName "mwc-list")

onSelected :: forall r i. (Event -> i) -> IProp r i
onSelected = handler (EventType "selected")

activatable :: forall r i. IProp r i
activatable = prop (PropName "activatable") true

multi :: forall r i. IProp r i
multi = prop (PropName "multi") true

mwc_list_item :: forall r w i. Node r w i
mwc_list_item = HE.element (ElemName "mwc-list-item")

hasMeta :: forall r i. IProp r i
hasMeta = prop (PropName "hasMeta") true

metaSlot :: forall r i. IProp r i
metaSlot = prop (PropName "slot") "meta"

mwc_check_list_item :: forall r w i. Node r w i
mwc_check_list_item = HE.element (ElemName "mwc-check-list-item")

left :: forall r i. IProp r i
left = prop (PropName "left") true