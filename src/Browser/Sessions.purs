module PureTabs.Browser.Sessions where

import Prelude

import Browser.Tabs (TabId(..))
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, runEffectFn2, runEffectFn3, runEffectFn4)

foreign import setTabValueImpl 
  :: forall r. EffectFn3 Number String r (Promise Unit)

setTabValue 
  :: forall r
   . TabId
  -> String
  -> r
  -> Aff Unit
setTabValue (TabId tid) key value = toAffE $ runEffectFn3 setTabValueImpl tid key value

foreign import removeTabValueImpl
  :: EffectFn2 Number String (Promise Unit)

removeTabValue
  :: TabId
  -> String
  -> Aff Unit
removeTabValue (TabId tid) key = toAffE $ runEffectFn2 removeTabValueImpl tid key

foreign import getTabValueImpl
  :: forall r. EffectFn4 (r -> Maybe r) (Maybe r) Number String (Promise (Maybe r))

getTabValue
  :: forall r
   . TabId
  -> String
  -> Aff (Maybe r)
getTabValue (TabId tid) key = toAffE $ runEffectFn4 getTabValueImpl Just Nothing tid key
