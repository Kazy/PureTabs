module PureTabs.Model.BackgroundEvent where 

import Browser.Tabs (Tab, TabId)
import Browser.Tabs.OnUpdated (ChangeInfo)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Show (class Show)
import PureTabs.Model.Group (GroupId)
import PureTabs.Model.GroupMapping (GroupData)
import PureTabs.Model.TabWithGroup (TabWithGroup)


data BackgroundEvent
  = BgInitialTabList (Array GroupData) (Array TabWithGroup)
  | BgInitializeGroups (Array GroupData)
  | BgTabCreated Tab (Maybe GroupId)
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

