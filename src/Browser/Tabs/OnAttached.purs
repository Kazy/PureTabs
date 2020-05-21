module Browser.Tabs.OnAttached where

import Browser.Tabs (TabId, WindowId)
import Browser.Utils (Listener2)
import Data.Unit (Unit)
import Effect (Effect)

type AttachInfo = {
  newWindowId :: WindowId,
  newPosition :: Int
}

foreign import addListener :: (Listener2 TabId AttachInfo) -> Effect Unit
foreign import removeListener :: (Listener2 TabId AttachInfo) -> Effect Unit
