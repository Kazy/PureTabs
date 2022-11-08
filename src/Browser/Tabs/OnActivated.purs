module Browser.Tabs.OnActivated (addListener, removeListener, ActiveInfo(..)) where

import Browser.Tabs (TabId, WindowId)
import Browser.Utils (Listener, UnregisteredListener, unwrapForeign, mkListenerOne)
import Control.Bind ((>=>))
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Data.Unit (Unit)
import Effect (Effect)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Prelude (bind)

newtype ActiveInfo
  = ActiveInfo
  { previousTabId :: Maybe TabId
  , tabId :: TabId
  , windowId :: WindowId
  }

derive instance newtypeActiveInfo :: Newtype ActiveInfo _

derive instance genActiveInfo :: Generic ActiveInfo _

instance showActiveInfo :: Show ActiveInfo where
  show = genericShow

instance encodeActiveInfo :: Encode ActiveInfo where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

instance decodeActiveInfo :: Decode ActiveInfo where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

foreign import addListener' :: (Listener Foreign) -> Effect Unit

addListener :: (UnregisteredListener ActiveInfo) -> Effect Unit
addListener listener = do
  lst <- mkListenerOne $ unwrapForeign >=> listener
  addListener' lst

foreign import removeListener :: (Listener ActiveInfo) -> Effect Unit
