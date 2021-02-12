module PureTabs.Model.Events (
  BackgroundEvent(..)
  , SidebarEvent(..)
  ) where

import Browser.Tabs (Tab, TabId, WindowId)
import Browser.Tabs.OnUpdated (ChangeInfo)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Show (class Show)
import PureTabs.Model.Group (GroupId)


data BackgroundEvent
  = BgInitialTabList (Array Tab)
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

derive instance genSidebarEvent :: Generic SidebarEvent _

instance showSidebarEvent :: Show SidebarEvent where
  show = genericShow
