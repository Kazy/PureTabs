module PureTabs.Model.GlobalState (
 ExtWindow
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
  , _tabIndex
  , _tabs
  , _tabWindowId
  , _windowIdToWindow
  , _windowIdToTabIdToTab
  , _windows
  , emptyWindow
  , initialGlobalState
  , initialTabsToGlobalState
  , addEmptyWindow
  , deleteWindow
  , createTab
  , updateTab
  , activateTab
  , moveTab
  , deleteTab
  , detachTab
  , attachTab
  , sendToTabPort
  , sendToWindowPort
  , tabFromWinIdAndTabId 
  , initializeWindowState
  ) where

import Browser.Runtime (Port, postMessageJson)
import Browser.Tabs (Tab(..), TabId, WindowId, showTabId)
import Control.Alt ((<|>))
import Control.Bind (join, bind, (>>=))
import Control.Category (identity, (<<<), (>>>))
import Control.Plus (empty) as A
import Data.Array (deleteAt, filter, foldl, fromFoldable, insertAt, mapWithIndex, sortBy, groupBy, (!!)) as A
import Data.Eq ((==), (/=))
import Data.Function (const, on, ($))
import Data.Functor (map, (<#>), (<$>))
import Data.Lens (Lens', Traversal', _Just, over, preview, set, view)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (head) as L
import Data.Array.NonEmpty (NonEmptyArray, head) as NEA
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe, maybe')
import Data.Monoid ((<>))
import Data.Ord (compare)
import Data.Show (show)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console (error)
import Effect.Exception.Unsafe (unsafeThrow)
import Prelude ((#))
import PureTabs.Model.Events (BackgroundEvent)

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
emptyWindow =
  { positions: A.empty
  , tabs: M.empty
  , port: Nothing
  }

_tabs :: forall a r. Lens' { tabs :: a | r } a
_tabs = prop (SProxy :: _ "tabs")

_port :: forall a r. Lens' { port :: a | r } a
_port = prop (SProxy :: _ "port")

_windows :: forall a r. Lens' { windows :: a | r } a
_windows = prop (SProxy :: _ "windows")

_title :: forall a r. Lens' { title :: a | r } a
_title = prop (SProxy :: _ "title")

_tabTitle :: Lens' Tab String
_tabTitle = _Newtype <<< _title

_index :: forall a r. Lens' { index :: a | r } a
_index = prop (SProxy :: _ "index")

_tabIndex :: Lens' Tab Int
_tabIndex = _Newtype <<< _index

_id :: forall a r. Lens' { id :: a | r } a
_id = prop (SProxy :: _ "id")

_tabId :: Lens' Tab TabId
_tabId = _Newtype <<< _id

_active :: forall a r. Lens' { active :: a | r } a
_active = prop (SProxy :: _ "active")

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

tabFromWinIdAndTabId :: WindowId -> TabId -> GlobalState -> Maybe Tab
tabFromWinIdAndTabId winId tabId = join <<< preview (_windowIdToTabIdToTab winId tabId)

_tabFromTabIdAndWindow :: GlobalState -> TabId -> Maybe Tab
_tabFromTabIdAndWindow s tabId =
  let
    allWindows = M.values s.windows

    allTabs = map (view _tabs) allWindows

    matchingTabId = map (M.lookup tabId) allTabs
  in
    join $ L.head matchingTabId


sendToTabPort :: Tab -> GlobalState -> BackgroundEvent -> Effect Unit
sendToTabPort tab state msg =
  case (preview (_portFromWindow tab) state) of 
       Just port -> postMessageJson port msg
       Nothing -> error $ "bg: no port found for tab id " <> (showTabId tab)

sendToWindowPort :: WindowId -> GlobalState -> BackgroundEvent -> Effect Unit
sendToWindowPort wid state event =
  case (preview (_portFromWindowId wid) state) of
    Just port -> postMessageJson port event
    Nothing -> error $ "bg: no port found for window id " <> (show wid)

initialTabsToGlobalState :: Array Tab -> GlobalState
initialTabsToGlobalState tabs = { windows: windows, detached: Nothing }
  where
  groupedTabs = A.groupBy (\(Tab t1) (Tab t2) -> t1.windowId == t2.windowId) tabs

  tabsToWindow :: NEA.NonEmptyArray Tab -> Tuple WindowId ExtWindow
  tabsToWindow tabs' =
    let
      windowId = (\(Tab t) -> t.windowId) $ NEA.head tabs'

      window =
        { tabs: M.fromFoldable $ tabs' <#> \(Tab t) -> Tuple t.id (Tab t)
        , port: Nothing
        , positions: (\(Tab t) -> t.id) <$> A.sortBy (compare `on` \(Tab t) -> t.index) (A.fromFoldable tabs')
        }
    in
      Tuple windowId window

  windows = M.fromFoldable $ (tabsToWindow <$> groupedTabs)


addEmptyWindow :: WindowId -> GlobalState -> GlobalState
addEmptyWindow winId = (over (_windows <<< at winId)) (_ <|> (Just emptyWindow))

deleteWindow :: WindowId -> GlobalState -> GlobalState
deleteWindow winId state = state { windows = M.delete winId state.windows }


createTab :: Tab -> GlobalState -> GlobalState
createTab (Tab t) s = s { windows = M.update updateWindow t.windowId windows }

  where

  windows = case M.lookup t.windowId s.windows of
                 Nothing -> M.insert t.windowId emptyWindow s.windows
                 Just _ -> s.windows

  updateWindow :: ExtWindow -> Maybe ExtWindow
  updateWindow win =
    -- this will delete the window if there is an issue with the position..
    -- not the best solution but we can't really recover from it anyway.
    (A.insertAt t.index t.id win.positions)
      <#> \newPos ->
        win
        { positions = newPos
        , tabs = M.insert t.id (Tab t) win.tabs
        }


updateTab :: Tab -> GlobalState -> GlobalState
updateTab tab = 
  -- update by replacing the tab only if it already exists
  (over (_tabFromWindow tab) (map $ const tab))
  -- or update the currently detached tab
    >>> ( \s -> case s.detached of
          Just (Tab tab')
            | (view _tabId tab) == tab'.id -> s { detached = Just (Tab tab') }
          _ -> s
      )


moveTab :: Int -> Int -> WindowId -> GlobalState -> GlobalState
moveTab fromIndex toIndex windowId state = 
  let 
      -- Update the state by moving the tab at `fromIndex` to `toIndex`.
      newState = state # over ((_windowIdToWindow windowId) <<< _positions) unsafeUpdatePositions

      -- Get the new positions for each tab based on the move just done.
      newPositions = newState # view ((_windowIdToWindow windowId) <<< _positions)
   in
     -- Update the new positions for each tab
     newState # over ((_windowIdToWindow windowId) <<< _tabs) (updateTabsIndex newPositions) 

  where
    -- | Move an element from `from` to `to` in array `arr`.
    moveElement :: forall a. Int -> Int -> Array a -> Maybe (Array a)
    moveElement from to arr = do
      tab <- arr A.!! from
      A.deleteAt from arr >>= A.insertAt to tab

    -- | Update the positions tabs
    unsafeUpdatePositions :: Array TabId -> Array TabId
    unsafeUpdatePositions =
      (moveElement fromIndex toIndex)
      -- The indexes should exist, we need to revisit the code if it doesn't
      >>> (maybe' (\_ -> unsafeThrow "bg: invalid indexes during moveTab") identity)

    -- | Update the index of the tab given the positions.
    -- | This is done by folding over a map of index update function applied to all tabs.
    updateTabsIndex :: Array TabId -> M.Map TabId Tab -> M.Map TabId Tab
    updateTabsIndex positions tabs =
      let
        modifyFuncs :: Array (M.Map TabId Tab -> M.Map TabId Tab)
        modifyFuncs = A.mapWithIndex (\idx tid' -> set (at tid' <<< _Just <<< _Newtype <<< _index) idx) positions
      in
        A.foldl (#) tabs modifyFuncs

activateTab :: WindowId -> (Maybe TabId) -> TabId -> GlobalState -> GlobalState
activateTab winId previousTabId newTabId state =
    let
      prevTab :: Maybe Tab
      prevTab = previousTabId >>= \ptid -> join $ preview (_windowIdToTabIdToTab winId ptid) state

      prevTabF :: GlobalState -> GlobalState
      prevTabF = maybe identity (\t -> set (_activeTab t) false) prevTab

      newTab = join $ preview (_windowIdToTabIdToTab winId newTabId) state

      newTabF :: GlobalState -> GlobalState
      newTabF = maybe identity (\t -> set (_activeTab t) true) newTab

      _activeTab t = (_tabFromWindow t) <<< _Just <<< _Newtype <<< _active
    in
      (prevTabF >>> newTabF) state


deleteTab :: WindowId -> TabId -> GlobalState -> GlobalState
deleteTab winId tabId = 
  (set (_windowIdToTabIdToTab winId tabId) Nothing)
    >>> over (_windowIdToWindow winId <<< _positions) (A.filter ((/=) tabId))


detachTab :: WindowId -> TabId -> GlobalState -> GlobalState
detachTab winId tabId state =
  case preview (_windowIdToTabIdToTab winId tabId) state of
    Just (Just tab) -> do
      state # (deleteTab winId tabId) >>> \s -> s { detached = Just tab } 
    -- XXX: We're losing the information that we couldn't fetch the tab.
    -- This shouldn't happen, but I don't see how to go around it. We don't
    -- have a (typed) proof that a given tab exists for a window id and a tab
    -- id, so let's just assume everything is well behaved.
    -- The other solution is to first do a read, then a write, and return an
    -- effect where we can throw.
    _ -> state


attachTab :: WindowId -> TabId -> Int -> GlobalState -> GlobalState
attachTab winId tabId newPosition state =
  case state.detached of 
       Just (Tab tab) -> 
         let 
             newTab = Tab (tab { windowId = winId, index = newPosition })
         in 
         state # (createTab newTab) >>> (_ { detached = Nothing})
       _ -> state


-- | Set the port of a new window connecting. If the window doesn't exist,
-- | initialize it with new data.
initializeWindowState :: WindowId -> Port -> GlobalState -> GlobalState
initializeWindowState winId port = 
  over (_windows <<< (at winId)) (\win -> Just $ set _port (Just port) (fromMaybe emptyWindow win))
