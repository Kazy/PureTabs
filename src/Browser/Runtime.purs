module Browser.Runtime (Port, connect, onConnectAddListener, postMessage, onMessageAddListener) where

import Prelude (Unit)
import Effect (Effect)
  
foreign import data Port :: Type

foreign import connect :: Effect Port

foreign import onConnectAddListener :: (Port -> Effect Unit) -> Effect Unit

foreign import postMessage :: Port -> String -> Effect Unit

foreign import onMessageAddListener :: Port -> (String -> Effect Unit) -> Effect Unit
