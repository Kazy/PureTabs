module Browser.Tabs.OnRemoved (RemoveInfo, addListener, removeListener) where

import Prelude (Unit)
import Effect (Effect)
import Browser.Tabs (WindowId, TabId)
import Browser.Utils (Listener2)

type RemoveInfo = {
  windowId :: WindowId,
  isWindowClosing :: Boolean
}

foreign import addListener :: (Listener2 TabId RemoveInfo) -> Effect Unit

foreign import removeListener :: (Listener2 TabId RemoveInfo) -> Effect Unit
