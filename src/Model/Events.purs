module PureTabs.Model.Events (
  BackgroundEvent(..)
  , SidebarEvent(..)
  , TabWithGroup(..)
  , GroupMapping(..)
  , groupMapping
  ) where

import Browser.Tabs (Tab, TabId, WindowId)
import Browser.Tabs.OnUpdated (ChangeInfo)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.Maybe (Maybe)
import Data.Show (class Show)
import PureTabs.Model.Group (GroupId)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)


data TabWithGroup
  = TabWithGroup Tab (Maybe GroupId)

derive instance genTabWithGroup :: Generic TabWithGroup _

instance showTabWithGroup :: Show TabWithGroup where 
  show = genericShow

instance encodeTabWithGroup :: Encode TabWithGroup where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

instance decodeTabWithGroup :: Decode TabWithGroup where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x


newtype GroupMapping
  = GroupMapping { groupId :: GroupId
                 , name :: String
                 }

groupMapping :: GroupId -> String -> GroupMapping
groupMapping gid name = GroupMapping { groupId: gid, name: name }

derive instance genGroupMapping :: Generic GroupMapping _
derive instance newtypeGroupMapping :: Newtype GroupMapping _

instance showGroupMapping :: Show GroupMapping where 
  show = genericShow

instance encodeGroupMapping :: Encode GroupMapping where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

instance decodeGroupMapping :: Decode GroupMapping where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x


data BackgroundEvent
  = BgInitialTabList (Array GroupMapping) (Array TabWithGroup)
  | BgTabCreated Tab
  | BgTabDeleted TabId
  | BgTabUpdated TabId ChangeInfo Tab
  | BgTabMoved TabId Int Int
  | BgTabActivated (Maybe TabId) TabId
  | BgTabAttached Tab
  | BgTabDetached TabId
  | BgGroupDeleted GroupId (Maybe TabId)

derive instance genBackgroundEvent :: Generic BackgroundEvent _

instance showBackgroundEvent :: Show BackgroundEvent where
  show = genericShow

data SidebarEvent
  = SbDeleteTab TabId
  | SbActivateTab TabId
  | SbCreateTab (Maybe TabId)
  | SbMoveTab TabId Int
  | SbDetacheTab
  | SbHasWindowId WindowId
  | SbSelectedGroup (Array TabId)
  | SbDeletedGroup GroupId (Array TabId)
  | SbChangeTabGroup TabId (Maybe GroupId)
  | SbCreatedGroup GroupId String
  | SbRenamedGroup GroupId String
  | SbMovedGroup GroupId Int

derive instance genSidebarEvent :: Generic SidebarEvent _

instance showSidebarEvent :: Show SidebarEvent where
  show = genericShow
