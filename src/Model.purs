module PureTabs.Model
  ( Window
  , GlobalState
  , _tabs
  , _port
  , _windows
  , _portFromWindow
  , _tabFromWindow
  , _tabFromTabIdAndWindow
  , initialGlobalState
  , BackgroundEvent(..)
  , SidebarEvent(..)
  ) where

import Browser.Runtime (Port)
import Browser.Tabs (TabId, WindowId, Tab(..))
import Control.Alt (map)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', Traversal', _Just, filtered, itoListOf, toListOf, view)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.List (List, catMaybes)
import Data.Map (Map, empty, lookup, member, values)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Show (class Show)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Prelude ((<<<))

type Window
  = { tabs :: Map TabId Tab
    , port :: Maybe Port
    }

_tabs :: forall a r. Lens' { tabs :: a | r } a
_tabs = prop (SProxy :: SProxy "tabs")

_port :: forall a r. Lens' { port :: a | r } a
_port = prop (SProxy :: SProxy "port")

type GlobalState
  = { windows :: Map WindowId Window
    }

_windows :: forall a r. Lens' { windows :: a | r } a
_windows = prop (SProxy :: SProxy "windows")

_portFromWindow :: Tab -> Traversal' GlobalState Port
_portFromWindow tab' = _windows <<< (at tab.windowId) <<< _Just <<< _port <<< _Just
  where
  tab = unwrap tab'

_tabFromWindow :: Tab -> Traversal' GlobalState (Maybe Tab)
_tabFromWindow tab' = _windows <<< (at tab.windowId) <<< _Just <<< _tabs <<< (at tab.id)
  where
  tab = unwrap tab'

_tabFromTabIdAndWindow :: GlobalState -> TabId -> List Tab
_tabFromTabIdAndWindow s tabId =
  let
    allWindows = values s.windows

    allTabs = map (view _tabs) allWindows

    matchingTabId = map (lookup tabId) allTabs
  in
    catMaybes matchingTabId

{-- (values . map (view _tabs) . map (lookup tabId)) s.windows --}
initialGlobalState :: GlobalState
initialGlobalState =
  { windows: empty
  }

data BackgroundEvent
  = BgTabCreated Tab
  | BgTabDeleted TabId
  | BgTabMoved
  | BgTabActived TabId
  | BgTabAttached Tab
  | BgTabDetached TabId
  | BgTabHighlighted
  | BgTabReplaced
  | BgTabZoomChanged

derive instance genBackgroundEvent :: Generic BackgroundEvent _

instance showBackgroundEvent :: Show BackgroundEvent where
  show = genericShow

data SidebarEvent
  = SbTabDeleted TabId
  | SbTabCreated
  | SbTabMoved
  | SbTabDetached
  | SbGroupCreated
  | SbGroupDeleted
  | SbGroupRenamed
  | SbHasWindowId WindowId

derive instance genSidebarEvent :: Generic SidebarEvent _

instance showSidebarEvent :: Show SidebarEvent where
  show = genericShow
