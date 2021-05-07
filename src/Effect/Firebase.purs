module Effect.Firebase where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Argonaut (Json)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)

foreign import data Firestore :: Type
foreign import data DocRef :: Type
foreign import data CollectionRef :: Type

type Path = String

foreign import firestore :: Effect Firestore
foreign import doc :: Firestore -> Path -> Effect DocRef
foreign import readDocPromise :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> DocRef -> Effect (Promise (Maybe Json))

readDoc :: DocRef -> Aff (Maybe Json)
readDoc = toAffE <<< readDocPromise Just Nothing

foreign import collection :: Firestore -> Path -> Effect CollectionRef
foreign import addToCollectionPromise :: CollectionRef -> Json -> Effect (Promise DocRef)

addToCollection :: CollectionRef -> Json -> Aff DocRef
addToCollection colRef json = toAffE $ addToCollectionPromise colRef json

foreign import readCollectionPromise :: CollectionRef -> Effect (Promise (Array Json))

readCollection :: CollectionRef -> Aff (Array Json)
readCollection = toAffE <<< readCollectionPromise