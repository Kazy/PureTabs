module PureTabs.Browser.Storage (storageLocalGet, storageLocalSet) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)

foreign import storageLocalGetImpl
  :: forall r. EffectFn3 String (r -> Maybe r) (Maybe r) (Promise r)

storageLocalGet :: forall r. String -> Aff (Maybe r)
storageLocalGet keys = toAffE $ runEffectFn3 storageLocalGetImpl keys Just Nothing

foreign import storageLocalSetImpl
  :: forall r. EffectFn2 String r (Promise Unit)

storageLocalSet :: forall r. String -> r -> Aff Unit
storageLocalSet key value = toAffE $ runEffectFn2 storageLocalSetImpl key value
