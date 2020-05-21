module Browser.Windows.OnCreated (addListener, removeListener) where

import Browser.Utils (Listener)
import Browser.Windows (Window)
import Data.Unit (Unit)
import Effect (Effect)

foreign import addListener :: (Listener Window) -> Effect Unit

foreign import removeListener :: (Listener Window) -> Effect Unit

