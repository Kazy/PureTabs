module Browser.Tabs.OnDetached where

import Browser.Tabs (TabId, WindowId)
import Browser.Utils (Listener2)
import Data.Unit (Unit)
import Effect (Effect)

type DetachInfo = {
  oldWindowId :: WindowId,
  oldPosition :: Int
}

foreign import addListener :: (Listener2 TabId DetachInfo) -> Effect Unit
foreign import removeListener :: (Listener2 TabId DetachInfo) -> Effect Unit
