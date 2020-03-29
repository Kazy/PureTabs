module Browser.Tabs.OnCreated (Listener, ListenerRef, mkListener, addListener, removeListener) where

import Prelude
import Effect (Effect)
import Browser.Tabs (Tab)


foreign import data ListenerRef :: Type

type Listener = (Tab -> Effect Unit)


foreign import mkListener
  :: Listener -> Effect ListenerRef

foreign import addListener
  :: ListenerRef -> Effect Unit

foreign import removeListener
  :: ListenerRef -> Effect Unit
