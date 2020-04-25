module Browser.Tabs.OnCreated (addListener, removeListener) where

import Browser.Tabs (Tab)
import Browser.Utils (Listener, UnregisteredListener, mkListenerOne, unwrapForeign)
import Control.Bind ((>=>))
import Effect (Effect)
import Foreign (Foreign)
import Prelude (Unit, bind, ($))

foreign import addListenerImpl :: (Listener Foreign) -> Effect Unit

addListener :: (UnregisteredListener Tab) -> Effect Unit
addListener listener = do
  lst <- mkListenerOne $ unwrapForeign >=> listener
  addListenerImpl lst

foreign import removeListener :: (Listener Tab) -> Effect Unit
