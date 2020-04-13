module Browser.Utils
  ( UnregisteredListener
  , UnregisteredListener2
  , Listener
  , Listener2
  , mkListenerUnit
  , mkListenerOne
  , mkListenerTwo
  ) where

import Effect (Effect)
import Prelude (Unit)

type UnregisteredListener a
  = (a -> Effect Unit)

type UnregisteredListener2 a b
  = (a -> b -> Effect Unit)

newtype Listener a
  = Listener (UnregisteredListener a)

newtype Listener2 a b
  = Listener2 (UnregisteredListener2 a b)

foreign import mkListenerUnit :: (Effect Unit) -> Effect (Listener Unit)

foreign import mkListenerOne :: forall a. (UnregisteredListener a) -> Effect (Listener a)

foreign import mkListenerTwo :: forall a b. (UnregisteredListener2 a b) -> Effect (Listener2 a b)
