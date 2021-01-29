module PureTabs.Model
  ( ExtWindow
  , GlobalState
  , Group
  , GroupId
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
  , initialTabListToGlobalState
  , BackgroundEvent(..)
  , SidebarEvent(..)
  ) where

import Browser.Runtime (Port)
import Browser.Tabs (Tab(..), TabId, WindowId)
import Browser.Tabs.OnUpdated (ChangeInfo)
import Control.Bind (join)
import Control.Category ((<<<))
import Control.Plus (empty) as A
import Data.Array (sortBy, singleton, fromFoldable) as A
import Data.Eq ((==))
import Data.Function (on, ($))
import Data.Functor (map, (<#>), (<$>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', Traversal', _Just, view)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List, groupBy, head) as L
import Data.List.NonEmpty (NonEmptyList, head) as NEL
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (compare)
import Data.Show (class Show)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))

type GlobalState
  = { windows :: M.Map WindowId ExtWindow
    , detached :: Maybe Tab
    }

initialGlobalState :: GlobalState
initialGlobalState =
  { windows: M.empty
  , detached: Nothing
  }

newtype GroupId
  = GroupId Int

type Group
  = { id :: GroupId, name :: String }

newGroup :: Int -> (Maybe String) -> Group
newGroup gid name = { id: GroupId gid, name: fromMaybe "Unnamed" name }

type ExtWindow
  = { positions :: Array TabId
    , tabs :: M.Map TabId Tab
    , port :: Maybe Port
    , groups :: Array Group
    , tabToGroup :: M.Map TabId GroupId
    , currentGroup :: GroupId
    }

emptyWindow :: ExtWindow
emptyWindow =
  { positions: A.empty
  , tabs: M.empty
  , port: Nothing
  , groups: A.singleton (newGroup 1 Nothing)
  , tabToGroup: M.empty
  , currentGroup: GroupId 1
  }

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
    join $ L.head matchingTabId

initialTabListToGlobalState :: L.List Tab -> GlobalState
initialTabListToGlobalState tabs = { windows: windows, detached: Nothing }
  where
  groupedTabs = L.groupBy (\(Tab t1) (Tab t2) -> t1.windowId == t2.windowId) tabs

  tabsToWindow :: NEL.NonEmptyList Tab -> Tuple WindowId ExtWindow
  tabsToWindow tabs' =
    let
      windowId = (\(Tab t) -> t.windowId) $ NEL.head tabs'

      window =
        { tabs: M.fromFoldable $ tabs' <#> \(Tab t) -> Tuple t.id (Tab t)
        , port: Nothing
        , positions: (\(Tab t) -> t.id) <$> A.sortBy (compare `on` \(Tab t) -> t.index) (A.fromFoldable tabs')
        , groups: A.singleton (newGroup 1 Nothing)
        , tabToGroup: M.fromFoldable $ tabs' <#> \(Tab t) -> Tuple t.id (GroupId 1)
        , currentGroup: GroupId 1
        }
    in
      Tuple windowId window

  windows = M.fromFoldable $ (tabsToWindow <$> groupedTabs)

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
  | SbCreateTab (Maybe TabId)
  | SbMoveTab TabId Int
  | SbDetacheTab
  | SbCreatedGroup
  | SbDeleteGroup
  | SbRenameGroup
  | SbHasWindowId WindowId

derive instance genSidebarEvent :: Generic SidebarEvent _

instance showSidebarEvent :: Show SidebarEvent where
  show = genericShow
