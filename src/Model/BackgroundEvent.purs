module PureTabs.Model.BackgroundEvent where 

import Browser.Tabs (Tab, TabId)
import Browser.Tabs.OnUpdated (ChangeInfo)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe)
import Data.Show (class Show)
import PureTabs.Model.Group (GroupId)
import PureTabs.Model.GroupMapping (GroupData)
import PureTabs.Model.TabWithGroup (TabWithGroup)


data BackgroundEvent
  = BgInitialTabList (Array GroupData) (Array TabWithGroup)
  | BgInitializeGroups (Array GroupData)
  | BgTabCreated Tab

  -- Initially we were assigning the group from the Sidebar each time a tab is created. The issue is
  -- that to avoid creating the tab in Aff (see onTabCreated method) we're splitting the action of
  -- creating a tab and assigning it a group in two different messages. Now the problem comes with
  -- the asynchronicity of each action:
  -- - We're first creating a tab with no group assigned.
  -- - We're then sending a message to update the group of each tab.
  -- The missing piece in between is that when the Sidebar received the tab creation event, it
  -- didn't have the tab's group assignation and was assigning a default one. It would then receive
  -- a message to switch the group, but a message to the background was already sent to update the
  -- group of the tab to the initial one. So while the state of the current session was correct, at
  -- the next session the restored tabs would lose their group.
  --
  -- Hence this solution: 
  -- | GroupId of Nothing means we ask the Sidebar to assign the group (by using SbChangeTabGroup).
  -- | Just GroupId means we tell the Sidebar the group of the tab (i.e. we're restoring a tab).
  | BgAssignTabToGroup TabId (Maybe GroupId)
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

