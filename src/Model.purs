module PureTabs.Model
  ( ExtWindow
  , GlobalState
  , _active
  , _id
  , _index
  , _port
  , _portFromWindow
  , _portFromWindowId
  , _positions
  , _tabFromTabIdAndWindow
  , _tabFromWindow
  , _tabId
  , _tabs
  , _tabWindowId
  , _windowIdToWindow
  , _windowIdToTabIdToTab
  , _windows
  , emptyWindow
  , initialGlobalState
  , tabsToGlobalState
  , BackgroundEvent(..)
  , SidebarEvent(..)
  ) where

import Browser.Runtime (Port)
import Browser.Tabs (TabId(..), WindowId, Tab(..))
import Browser.Tabs.OnUpdated (ChangeInfo(..))
import Control.Alternative (empty)
import Control.Bind (join)
import Control.Category ((>>>), (<<<))
import Control.Plus (empty) as A
import Data.Array (sortBy, singleton) as A
import Data.Function (on, ($))
import Data.Functor (map)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', Traversal', _Just, view)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), catMaybes, concat, head, singleton)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Ord (compare)
import Data.Show (class Show)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Tuple.Nested ((/\))

type GlobalState
  = { windows :: M.Map WindowId ExtWindow
    , detached :: Maybe Tab
    }

initialGlobalState :: GlobalState
initialGlobalState =
  { windows: M.empty
  , detached: Nothing
  }

type ExtWindow
  = { positions :: Array TabId
    , tabs :: M.Map TabId Tab
    , port :: Maybe Port
    }

emptyWindow :: ExtWindow
emptyWindow = { positions: A.empty, tabs: M.empty, port: Nothing }

_tabs :: forall a r. Lens' { tabs :: a | r } a
_tabs = prop (SProxy :: _ "tabs")

_port :: forall a r. Lens' { port :: a | r } a
_port = prop (SProxy :: _ "port")

_windows :: forall a r. Lens' { windows :: a | r } a
_windows = prop (SProxy :: _ "windows")

_title :: forall a r. Lens' { title :: a | r } a
_title = prop (SProxy :: _ "title")

_index :: forall a r. Lens' { index :: a | r } a
_index = prop (SProxy :: _ "index")

_tabTitle :: Lens' Tab String
_tabTitle = _Newtype <<< _title

_id :: forall a r. Lens' { id :: a | r } a
_id = prop (SProxy :: _ "id")

_active :: forall a r. Lens' { active :: a | r } a
_active = prop (SProxy :: _ "active")

_tabId :: Lens' Tab TabId
_tabId = _Newtype <<< _id

_windowId :: forall a r. Lens' { windowId :: a | r } a
_windowId = prop (SProxy :: _ "windowId")

_positions :: forall a r. Lens' { positions :: a | r } a
_positions = prop (SProxy :: _ "positions")

_tabWindowId :: Lens' Tab WindowId
_tabWindowId = _Newtype <<< _windowId

_portFromWindow :: Tab -> Traversal' GlobalState Port
_portFromWindow (Tab tab) = _portFromWindowId tab.windowId

_portFromWindowId :: WindowId -> Traversal' GlobalState Port
_portFromWindowId wid = _windowIdToWindow wid <<< _port <<< _Just

_windowIdToWindow :: WindowId -> Traversal' GlobalState ExtWindow
_windowIdToWindow wid = _windows <<< (at wid) <<< _Just

_tabFromWindow :: Tab -> Traversal' GlobalState (Maybe Tab)
_tabFromWindow (Tab tab) = _windowIdToWindow tab.windowId <<< _tabs <<< (at tab.id)

_windowIdToTabIdToTab :: WindowId -> TabId -> Traversal' GlobalState (Maybe Tab)
_windowIdToTabIdToTab wid tid = _windowIdToWindow wid <<< _tabs <<< (at tid)

_tabFromTabIdAndWindow :: GlobalState -> TabId -> Maybe Tab
_tabFromTabIdAndWindow s tabId =
  let
    allWindows = M.values s.windows

    allTabs = map (view _tabs) allWindows

    matchingTabId = map (M.lookup tabId) allTabs
  in
    join $ head matchingTabId

tabsToGlobalState :: List Tab -> GlobalState
tabsToGlobalState tabs = { windows: tabsToWindows tabs, detached: Nothing }
  where
  tabsToWindows :: List Tab -> M.Map WindowId ExtWindow
  tabsToWindows tabs' = M.fromFoldableWith merge $ map mapTab tabs'

  merge :: ExtWindow -> ExtWindow -> ExtWindow
  merge w1 w2 =
    let
      mergedMap = M.union w1.tabs w2.tabs
    in
      { tabs: mergedMap
      , port: Nothing
      -- TODO do that after building the state, to avoid going creating a new list each time
      , positions: (mapPositions >>> (A.sortBy (compare `on` snd)) >>> (map fst)) mergedMap
      }

  mapTab :: Tab -> Tuple WindowId ExtWindow
  mapTab (Tab t) = Tuple t.windowId { tabs: M.singleton t.id (Tab t), port: Nothing, positions: A.singleton t.id }

  mapPositions :: M.Map TabId Tab -> Array (Tuple TabId Int)
  mapPositions = M.toUnfoldableUnordered >>> (map \(Tuple tid (Tab t)) -> tid /\ t.index)

data BackgroundEvent
  = BgInitialTabList (Array Tab)
  | BgTabCreated Tab
  | BgTabDeleted TabId
  | BgTabUpdated TabId ChangeInfo Tab
  | BgTabMoved TabId Int Int
  | BgTabActivated (Maybe TabId) TabId
  | BgTabAttached Tab
  | BgTabDetached TabId
  | BgTabHighlighted
  | BgTabReplaced
  | BgTabZoomChanged

derive instance genBackgroundEvent :: Generic BackgroundEvent _

instance showBackgroundEvent :: Show BackgroundEvent where
  show = genericShow

data SidebarEvent
  = SbDeleteTab TabId
  | SbActivateTab TabId
  | SbCreateTab
  | SbMoveTab TabId Int
  | SbDetacheTab
  | SbCreatedGroup
  | SbDeleteGroup
  | SbRenameGroup
  | SbHasWindowId WindowId

derive instance genSidebarEvent :: Generic SidebarEvent _

instance showSidebarEvent :: Show SidebarEvent where
  show = genericShow
