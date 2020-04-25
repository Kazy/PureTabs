module Browser.Tabs.OnMoved (addListener, removeListener) where

import Browser.Tabs (TabId(..), WindowId)
import Browser.Utils (Listener2, UnregisteredListener2)
import Control.Alternative (pure)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Undefined (undefined)

type MoveInfo = { windowId :: WindowId
  , fromIndex :: Number
  , toIndex :: Number
} 

addListener :: (UnregisteredListener2 TabId MoveInfo) -> Effect Unit
addListener lst = undefined

removeListener :: (Listener2 TabId MoveInfo) -> Effect Unit
removeListener lst = undefined
