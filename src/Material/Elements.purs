module Material.Elements where

import Halogen (ElemName(..), PropName(..))
import Halogen.HTML.Core (HTML, text)
import Halogen.HTML.Elements (Leaf, Node)
import Halogen.HTML.Elements as HE
import Halogen.HTML.Properties (IProp, prop)

md_filled_button :: forall r w i. Node r w i
md_filled_button = HE.element (ElemName "md-filled-button")

md_outlined_button :: forall r w i. Node r w i
md_outlined_button = HE.element (ElemName "md-outlined-button")

hasIcon :: forall r i. IProp r i
hasIcon = prop (PropName "hasIcon") true

trailingIcon :: forall r i. IProp r i
trailingIcon = prop (PropName "trailingIcon") true

newtype IconName = IconName String

md_icon :: forall w i. IconName -> HTML w i
md_icon (IconName s)= HE.element (ElemName "md-icon") [] [ text s ]

md_filled_icon_button ∷ ∀ r w i. Node r w i
md_filled_icon_button = HE.element (ElemName "md-filled-icon-button")

md_outlined_icon_button ∷ ∀ r w i. Node r w i
md_outlined_icon_button = HE.element (ElemName "md-outlined-icon-button")

md_circular_progress :: forall r w i. Leaf r w i
md_circular_progress props = HE.element (ElemName "md-circular-progress") props []

fourColor :: forall r i. IProp r i
fourColor = prop (PropName "fourColor") true

indeterminate :: forall r i. IProp r i
indeterminate = prop (PropName "indeterminate") true

disabled :: forall r i. IProp r i
disabled = prop (PropName "disabled") true

md_list :: forall r w i. Node r w i
md_list = HE.element (ElemName "md-list")

md_list_item :: forall r w i. Node r w i
md_list_item = HE.element (ElemName "md-list-item")

md_divider :: forall r w i. Leaf r w i
md_divider props = HE.element (ElemName "md-divider") props []