module Browser.Tabs (
  WindowId
  , TabId(..)
  , Tab(..)
  , MoveProperties
  , CreateProperties
  , browserQuery
  , browserRemove
  , browserRemoveOne
  , browserUpdate
  , browserActivateTab
  , browserMoveTab
  , browserCreateTab
  , browserHideTabs
  , browserShowTabs
  , showTabId
  ) where

import Browser.Utils (unwrapForeign)
import Control.Alt (map)
import Control.Promise (Promise, toAffE)
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Number.Format (toString)
import Data.Ord (class Ord)
import Data.Show (class Show, show)
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

derive instance newtypeWindowId :: Newtype WindowId _

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

instance showTabIdInstance :: Show TabId where
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

showTabId :: Tab -> String
showTabId (Tab t) = show t.id

instance encodeTab :: Encode Tab where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

instance decodeTab :: Decode Tab where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x


type QueryRecord = 
  ( active :: Boolean
  , audible :: Boolean
  , autoDiscardable :: Boolean
  , cookieStoreId :: String
  , currentWindow :: Boolean
  , discarded :: Boolean
  , hidden :: Boolean
  , highlighted :: Boolean
  , index :: Int
  , muted :: Boolean
  , lastFocusedWindow :: Boolean
  , pinned :: Boolean
  , title :: String
  , url :: String
  , windowId :: Number
  )

foreign import queryImpl 
  :: forall r
   . { | r }
  -> Effect (Promise (Array Foreign))

browserQuery 
  :: forall r r2
   . Union r r2 QueryRecord
  => Record r
  -> Aff (Array Tab)
browserQuery query = do
  tabsArray <- toAffE $ queryImpl query
  parsed <- liftEffect $ traverse unwrapForeign tabsArray
  pure parsed

foreign import browserRemove' :: (Array Number) -> Effect (Promise Unit)

browserRemove :: (Array TabId) -> Aff Unit
browserRemove tabs =
  let
    tabIdsArray = map unwrap tabs
  in
    toAffE $ browserRemove' tabIdsArray
  where
  unwrap (TabId n) = n

browserRemoveOne :: TabId -> Aff Unit
browserRemoveOne tabId = browserRemove [tabId]

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

foreign import browserUpdate' :: forall given trash. Union given trash RowUpdateProperties => { | given } -> TabId -> Effect (Promise Tab)

browserUpdate :: forall prop b. Union prop b RowUpdateProperties => { | prop } -> TabId -> Aff Tab
browserUpdate props tabId = toAffE $ browserUpdate' props tabId


browserActivateTab :: TabId -> Aff Tab
browserActivateTab tabId = browserUpdate { active: true } tabId

type MoveProperties = {
  -- windowId :: Maybe WindowId
  index :: Int
}

foreign import browserMoveTab :: TabId -> MoveProperties -> Effect Unit


type CreateProperties = (
  active :: Boolean,
  cookieStoreId :: String,
  discarded :: Boolean,
  index :: Int,
  openerTabId :: TabId,
  openInReaderMode :: Boolean,
  pinned :: Boolean,
  title :: String,
  url :: String,
  windowId :: WindowId
)

foreign import browserCreateTab :: forall props trash. Union props trash CreateProperties => { | props } -> Effect Unit

foreign import browserHideTabs :: Array TabId -> Effect Unit

foreign import browserShowTabs :: Array TabId -> Effect Unit
