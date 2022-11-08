module Browser.Tabs.OnUpdated where

import Browser.Tabs (Tab, TabId)
import Browser.Utils (Listener3, UnregisteredListener3, mkListenerThree, unwrapForeign)
import Control.Alternative (pure)
import Control.Bind ((>>=))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Data.Tuple.Nested (Tuple3, tuple3, uncurry3)
import Effect (Effect)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Prelude (Unit, bind, ($))

type ChangeInfoRec
  = { attention :: Maybe Boolean
    , audible :: Maybe Boolean
    , discarded :: Maybe Boolean
    , favIconUrl :: Maybe String
    , hidden :: Maybe Boolean
    , isArticle :: Maybe Boolean
    -- mutedInfo :: Maybe MutedInfo,
    , pinned :: Maybe Boolean
    , status :: Maybe String
    , title :: Maybe String
    , url :: Maybe String
    }

newtype ChangeInfo
  = ChangeInfo ChangeInfoRec

derive instance newtypeChangeInfo :: Newtype ChangeInfo _

derive instance genChangeInfo :: Generic ChangeInfo _

instance showChangeInfo :: Show ChangeInfo where
  show = genericShow

instance encodeChangeInfo :: Encode ChangeInfo where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

instance decodeChangeInfo :: Decode ChangeInfo where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

foreign import addListener' :: (Listener3 TabId Foreign Foreign) -> Effect Unit

foreign import removeListener' :: (Listener3 TabId Foreign Foreign) -> Effect Unit

addListener :: (UnregisteredListener3 TabId ChangeInfo Tab) -> Effect Unit
addListener listener = do
  lst' <- mkListenerThree \tid cinfo tab -> (lst tid cinfo tab) >>= uncurry3 listener
  addListener' lst'

lst :: TabId -> Foreign -> Foreign -> Effect (Tuple3 TabId ChangeInfo Tab)
lst tid changeInfo tab = do
  cinfo <- unwrapForeign changeInfo
  t <- unwrapForeign tab
  pure $ tuple3 tid cinfo t
