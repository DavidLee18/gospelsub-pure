module Material.Elements where

import Prelude

import Halogen (ElemName(..))
import Halogen.HTML.Elements (Leaf)
import Halogen.HTML.Elements as HE

mwc_checkbox :: forall r w i. Leaf r w i
mwc_checkbox props = HE.element (ElemName "mwc-checkbox") props []