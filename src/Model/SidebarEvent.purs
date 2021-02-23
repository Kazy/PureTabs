module PureTabs.Model.SidebarEvent where 

import Browser.Tabs (TabId, WindowId)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Show (class Show)
import PureTabs.Model.Group (GroupId)



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
