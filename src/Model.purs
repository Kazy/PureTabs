module PureTabs.Model
  ( Window
  , GlobalState
  , _tabs
  , _port
  , _windows
  , _portFromWindow
  , _tabFromWindow
  , _tabWindowId
  , _tabId
  , _tabFromTabIdAndWindow
  , initialGlobalState
  , tabsToGlobalState
  , BackgroundEvent(..)
  , SidebarEvent(..)
  ) where

import Browser.Runtime (Port)
import Browser.Tabs (TabId, WindowId, Tab)
import Control.Alt (map)
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', Traversal', _Just, view)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List, catMaybes)
import Data.Map (Map, empty, fromFoldableWith, lookup, singleton, union, values)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (class Show)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
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

_title :: forall a r. Lens' { title :: a | r } a
_title = prop (SProxy :: SProxy "title")

_tabTitle :: Lens' Tab String
_tabTitle = _Newtype <<< _title

_id :: forall a r. Lens' { id :: a | r } a
_id = prop (SProxy :: SProxy "id")

_tabId :: Lens' Tab TabId
_tabId = _Newtype <<< _id

_windowId :: forall a r. Lens' { windowId :: a | r } a
_windowId = prop (SProxy :: SProxy "windowId")

_tabWindowId :: Lens' Tab WindowId
_tabWindowId = _Newtype <<< _windowId

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

initialGlobalState :: GlobalState
initialGlobalState =
  { windows: empty
  }

tabsToGlobalState :: List Tab -> GlobalState
tabsToGlobalState tabs = { windows: tabsToWindows tabs }
  where
  tabsToWindows :: List Tab -> Map WindowId Window
  tabsToWindows tabs' =
    fromFoldableWith
      (\v1 v2 -> { tabs: union v1.tabs v2.tabs, port: Nothing })
      $ map
          ( \t ->
              Tuple
                (view _tabWindowId t)
                { tabs: singleton (view _tabId t) t, port: Nothing }
          )
          tabs'

data BackgroundEvent
  = BgInitialTabList (Array Tab)
  | BgTabCreated Tab
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
