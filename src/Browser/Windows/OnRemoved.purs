module Browser.Windows.OnRemoved (addListener, removeListener) where

import Browser.Tabs (WindowId)
import Browser.Utils (Listener)
import Data.Unit (Unit)
import Effect (Effect)

foreign import addListener :: (Listener WindowId) -> Effect Unit

foreign import removeListener :: (Listener WindowId) -> Effect Unit
