module Browser.Tabs.OnMoved (addListener, removeListener, MoveInfo) where

import Browser.Tabs (TabId, WindowId)
import Browser.Utils (Listener2)
import Data.Unit (Unit)
import Effect (Effect)

type MoveInfo
  = { windowId :: WindowId
    , fromIndex :: Int
    , toIndex :: Int
    }

foreign import addListener :: (Listener2 TabId MoveInfo) -> Effect Unit

foreign import removeListener :: (Listener2 TabId MoveInfo) -> Effect Unit
