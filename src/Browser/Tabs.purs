module Browser.Tabs (WindowId, TabId(..), Tab(..), MoveProperties, query, remove, removeOne, update, activateTab, moveTab) where

import Browser.Utils (unwrapForeign)
import Control.Alt (map)
import Control.Bind ((>>=))
import Control.Promise (Promise, toAffE)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List, fromFoldable, toUnfoldable, (!!), singleton)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Number.Format (toString)
import Data.Ord (class Ord)
import Data.Show (class Show)
import Data.Traversable (traverse)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Prelude (bind, pure)
import Prim.Row (class Union)

newtype WindowId
  = WindowId Number

derive instance eqWindowId :: Eq WindowId

derive instance ordWindowId :: Ord WindowId

instance showWindowId :: Show WindowId where
  show (WindowId wid) = toString wid

derive instance genWindowId :: Generic WindowId _

instance encodeWindowId :: Encode WindowId where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

instance decodeWindowId :: Decode WindowId where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

newtype TabId
  = TabId Number

derive instance eqTabId :: Eq TabId

derive instance ordTabId :: Ord TabId

instance showTabId :: Show TabId where
  show (TabId wid) = toString wid

derive instance genTabId :: Generic TabId _

instance encodeTabId :: Encode TabId where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

instance decodeTabId :: Decode TabId where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

newtype Tab
  = Tab
  { active :: Boolean
  , attention :: Maybe Boolean
  , audible :: Maybe Boolean
  , autoDiscardable :: Maybe Boolean
  , cookieStoreId :: Maybe String
  , discarded :: Maybe Boolean
  , favIconUrl :: Maybe String
  , height :: Maybe Number
  , hidden :: Boolean
  , highlighted :: Boolean
  -- should be optional
  , id :: TabId
  , incognito :: Boolean
  , index :: Int
  , isArticle :: Maybe Boolean
  , isInReaderMode :: Boolean
  , lastAccessed :: Number
  , openerTabId :: Maybe TabId
  , pinned :: Boolean
  , sessionId :: Maybe String
  , status :: Maybe String
  -- create an enum for that successorTabId :: Maybe Number
  , title :: String
  , url :: Maybe String
  , width :: Maybe Number
  , windowId :: WindowId
  }

derive instance newtypeTab :: Newtype Tab _

derive instance genTab :: Generic Tab _

instance showTab :: Show Tab where
  show = genericShow

instance encodeTab :: Encode Tab where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

instance decodeTab :: Decode Tab where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

foreign import queryImpl :: Effect (Promise (Array Foreign))

query :: Aff (List Tab)
query = do
  tabsArray <- toAffE queryImpl
  let
    tabsList = fromFoldable tabsArray
  parsed <- liftEffect $ traverse unwrapForeign tabsList
  pure parsed

foreign import remove' :: (Array Number) -> Effect (Promise Unit)

remove :: (List TabId) -> Aff Unit
remove tabs =
  let
    tabIdsArray = toUnfoldable $ map unwrap tabs
  in
    toAffE $ remove' tabIdsArray
  where
  unwrap (TabId n) = n

removeOne :: TabId -> Aff Unit
removeOne tabId = remove (singleton tabId)

type RowUpdateProperties
  = ( active :: Boolean
    , autoDiscardable :: Boolean
    , highlighted :: Boolean
    , loadReplace :: Boolean
    , muted :: Boolean
    , openerTabId :: TabId
    , pinned :: Boolean
    , successorTabId :: TabId
    , url :: String
    )

foreign import update' :: forall given trash. Union given trash RowUpdateProperties => { | given } -> TabId -> Effect (Promise Tab)

update :: forall prop b. Union prop b RowUpdateProperties => { | prop } -> TabId -> Aff Tab
update props tabId = toAffE $ update' props tabId


activateTab :: TabId -> Aff Tab
activateTab tabId = update { active: true } tabId

type MoveProperties = {
  -- windowId :: Maybe WindowId
  index :: Int
}

foreign import moveTab :: TabId -> MoveProperties -> Effect Unit
