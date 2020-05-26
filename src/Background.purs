module PureTabs.Background where

import Browser.Runtime as Runtime
import Browser.Tabs (Tab(..), TabId, WindowId, query, removeOne, activateTab, moveTab, createTab)
import Browser.Tabs.OnActivated as OnActivated
import Browser.Tabs.OnAttached as OnAttached
import Browser.Tabs.OnCreated as OnCreated
import Browser.Tabs.OnDetached as OnDetached
import Browser.Tabs.OnMoved as OnMoved
import Browser.Tabs.OnRemoved as OnRemoved
import Browser.Tabs.OnUpdated as OnUpdated
import Browser.Utils (Listener, mkListenerOne, mkListenerTwo, mkListenerUnit)
import Browser.Windows (Window)
import Browser.Windows.OnCreated as WinOnCreated
import Browser.Windows.OnRemoved as WinOnRemoved
import Control.Alt (map, (<#>), (<$>), (<|>))
import Control.Alternative (empty, pure, (*>))
import Control.Bind ((=<<), (>>=))
import Control.Category (identity, (>>>))
import Data.Array as A
import Data.CommutativeRing ((+))
import Data.Eq ((/=), (==))
import Data.Foldable (for_)
import Data.Function (const, flip, (#))
import Data.Lens (_Just, over, preview, set, view)
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (List, foldMap, foldr)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.Unit (unit)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref as Ref
import Prelude (Unit, bind, ($), discard, (<<<))
import PureTabs.Model (BackgroundEvent(..), ExtWindow, GlobalState, SidebarEvent(..), _active, _index, _port, _portFromWindow, _portFromWindowId, _positions, _tabFromTabIdAndWindow, _tabFromWindow, _tabs, _windowIdToWindow, _windows, _windowIdToTabIdToTab, emptyWindow, tabsToGlobalState)

type Ports
  = Ref.Ref (List Runtime.Port)

main :: Effect Unit
main = do
  log "starting background"
  launchAff_ runMain
  where
  runMain :: Aff Unit
  runMain = do
    allTabs <- query
    liftEffect $ initializeBackground =<< (Ref.new $ tabsToGlobalState allTabs)

initializeBackground :: Ref.Ref GlobalState -> Effect Unit
initializeBackground ref = do
  (mkListenerOne $ onWindowCreated ref) >>= WinOnCreated.addListener
  (mkListenerOne $ onWindowRemoved ref) >>= WinOnRemoved.addListener
  onTabCreated ref # OnCreated.addListener
  (mkListenerTwo $ onTabDeleted ref) >>= OnRemoved.addListener
  onTabActived ref # OnActivated.addListener
  onTabUpdated ref # OnUpdated.addListener
  (mkListenerTwo $ onTabDetached ref) >>= OnDetached.addListener
  (mkListenerTwo $ onTabAttached ref) >>= OnAttached.addListener
  (mkListenerTwo $ onTabMoved ref) >>= OnMoved.addListener
  (mkListenerOne $ onConnect ref) >>= Runtime.onConnectAddListener

onWindowCreated :: (Ref.Ref GlobalState) -> Window -> Effect Unit
onWindowCreated ref { id: winId } =
  (log $ "bg: created window " <> (show winId))
    *> (ref # Ref.modify_ (over (_windows <<< at winId) (_ <|> (Just emptyWindow))))

onWindowRemoved :: (Ref.Ref GlobalState) -> WindowId -> Effect Unit
onWindowRemoved ref winId =
  (log $ "bg: deleted window " <> (show winId))
    *> (ref # Ref.modify_ \s -> s { windows = M.delete winId s.windows })

onTabCreated :: (Ref.Ref GlobalState) -> Tab -> Effect Unit
onTabCreated stateRef (Tab tab) = do
  log $ "bg: created tab " <> show tab.id
  state <-
    Ref.modify (insertTab (Tab tab)) stateRef
  case (preview (_portFromWindow (Tab tab)) state) of
    Nothing -> pure unit
    Just port -> Runtime.postMessageJson port $ BgTabCreated (Tab tab)
  where
  -- | insert a tab, creating the window and updating the position
  insertTab :: Tab -> GlobalState -> GlobalState
  insertTab (Tab t) s =
    let
      windows = case M.lookup t.windowId s.windows of
        Nothing -> M.insert t.windowId emptyWindow s.windows
        Just _ -> s.windows
    in
      s { windows = M.update updateWindow t.windowId windows }
    where
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

onTabUpdated :: (Ref.Ref GlobalState) -> TabId -> OnUpdated.ChangeInfo -> Tab -> Effect Unit
onTabUpdated stateRef tid cinfo tab' = do
  log $ "bg: updated tab " <> show tid
  state <- Ref.modify (updateTab tab') stateRef
  case (preview (_portFromWindow tab') state) of
    Nothing -> pure unit
    Just port -> Runtime.postMessageJson port $ BgTabUpdated tid cinfo tab'
  where
  updateTab :: Tab -> GlobalState -> GlobalState
  updateTab (Tab t) =
    -- update by replacing the tab only if it already exists
    (over (_tabFromWindow (Tab t)) (map $ const (Tab t)))
      -- or update the currently detached tab
      
      >>> ( \s -> case s.detached of
            Just (Tab t')
              | t.id == t'.id -> s { detached = Just (Tab t') }
            _ -> s
        )

onTabMoved :: (Ref.Ref GlobalState) -> TabId -> OnMoved.MoveInfo -> Effect Unit
onTabMoved ref tid minfo = do
  log $ "bg: moved tab " <> show tid
  s <- Ref.modify (updateState minfo) ref
  case (preview (_portFromWindowId minfo.windowId) s) of
    Nothing -> pure unit
    Just port -> Runtime.postMessageJson port $ BgTabMoved tid minfo.fromIndex minfo.toIndex
  where
  updateState :: OnMoved.MoveInfo -> GlobalState -> GlobalState
  updateState minfo' state =
    let
      newState = updatePositions minfo' state

      newPositions :: Array TabId
      newPositions = view ((_windowIdToWindow minfo'.windowId) <<< _positions) newState
    in
      over ((_windowIdToWindow minfo'.windowId) <<< _tabs) (updateTabsIndex newPositions) newState

  updatePositions :: OnMoved.MoveInfo -> GlobalState -> GlobalState
  updatePositions minfo' = over ((_windowIdToWindow minfo'.windowId) <<< _positions) $ unsafeUpdatePositions minfo'

  -- | given a move info, update the positions tabs
  unsafeUpdatePositions :: OnMoved.MoveInfo -> Array TabId -> Array TabId
  unsafeUpdatePositions minfo' =
    (moveElement minfo'.fromIndex minfo'.toIndex)
      -- the indexes should exist, we need to revisit the code if it doesn't
      
      >>> (maybe' (\_ -> unsafeThrow "invalid indexes") identity)

  moveElement :: forall a. Int -> Int -> Array a -> Maybe (Array a)
  moveElement from to arr = do
    tab <- arr A.!! from
    A.deleteAt from arr >>= A.insertAt to tab

  -- | update the index of the tab given the positions
  updateTabsIndex :: Array TabId -> M.Map TabId Tab -> M.Map TabId Tab
  updateTabsIndex positions tabs =
    let
      modifyFuncs :: Array (M.Map TabId Tab -> M.Map TabId Tab)
      modifyFuncs = A.mapWithIndex (\idx tid' -> set (at tid' <<< _Just <<< _Newtype <<< _index) idx) positions
    in
      A.foldl (#) tabs modifyFuncs

onTabActived :: (Ref.Ref GlobalState) -> OnActivated.ActiveInfo -> Effect Unit
onTabActived stateRef (OnActivated.ActiveInfo aInfo) = do
  log $ "bg: activated tab " <> show aInfo.tabId
  state <- Ref.modify (updateGlobalState aInfo.previousTabId aInfo.tabId) stateRef
  case (preview (_portFromWindowId aInfo.windowId) state) of
    Nothing -> pure unit
    Just port -> Runtime.postMessageJson port $ BgTabActivated aInfo.previousTabId aInfo.tabId
  where
  updateGlobalState :: (Maybe TabId) -> TabId -> GlobalState -> GlobalState
  updateGlobalState prev new state =
    let
      -- TODO: we have the windowId, we can directly get the tab from that
      -- without using _tabFromTabIdAndWindow that goes through all the windows.
      prevTab = prev >>= _tabFromTabIdAndWindow state

      prevTabF :: GlobalState -> GlobalState
      prevTabF = maybe identity (\t -> set (_activeTab t) false) prevTab

      newTab = _tabFromTabIdAndWindow state new

      newTabF :: GlobalState -> GlobalState
      newTabF = maybe identity (\t -> set (_activeTab t) true) newTab

      _activeTab t = (_tabFromWindow t) <<< _Just <<< _Newtype <<< _active
    in
      (prevTabF >>> newTabF) state

stateDeleteTab :: WindowId -> TabId -> GlobalState -> GlobalState
stateDeleteTab wid tid =
  ( (set (_windowIdToTabIdToTab wid tid) Nothing)
      >>> over (_windowIdToWindow wid <<< _positions) (A.filter ((/=) tid))
  )

deleteTab :: (Ref.Ref GlobalState) -> WindowId -> TabId -> Effect Unit
deleteTab stateRef wid tid = do
  log $ "bg: deleted tab " <> show tid
  state <- Ref.modify (stateDeleteTab wid tid) stateRef
  let
    port = preview (_portFromWindowId wid) state
  maybe (pure unit) (\p -> Runtime.postMessageJson p (BgTabDeleted tid)) port

onTabDeleted :: (Ref.Ref GlobalState) -> TabId -> OnRemoved.RemoveInfo -> Effect Unit
onTabDeleted stateRef tabId info = deleteTab stateRef info.windowId tabId

onTabDetached :: (Ref.Ref GlobalState) -> TabId -> OnDetached.DetachInfo -> Effect Unit
onTabDetached stateRef tabId info = do
  log $ "bg: detached tab " <> show tabId
  oldState <- Ref.read stateRef
  case preview (_windowIdToTabIdToTab info.oldWindowId tabId) oldState of
    Just (Just tab) -> do
      deleteTab stateRef info.oldWindowId tabId
      Ref.modify_ (_ { detached = Just tab }) stateRef
    _ -> throw $ "tab " <> (show tabId) <> " not found, shouldn't happen"

onTabAttached :: (Ref.Ref GlobalState) -> TabId -> OnAttached.AttachInfo -> Effect Unit
onTabAttached stateRef tid info = do
  log $ "bg: attached tab " <> show tid
  state <- Ref.read stateRef
  case state.detached of
    Just (Tab tab) ->
      let
        newTab = Tab (tab { windowId = info.newWindowId, index = info.newPosition })
      in
        onTabCreated stateRef newTab
          *> Ref.modify_ (_ { detached = Nothing }) stateRef
    _ -> throw $ "tab " <> (show tid) <> " doesn't exist in the state, this shouldn't happen"

onConnect :: (Ref.Ref GlobalState) -> Runtime.Port -> Effect Unit
onConnect stateRef port = do
  -- create a temporary listener ref that will only be held until the sidebar has sent its current window
  listenerRef <- Ref.new Nothing
  initialListener <-
    Runtime.onMessageJsonAddListener port $ windowListener
      $ onNewWindowId port stateRef listenerRef
  -- XXX: is it possible a message arrive *before* this is executed ? 
  -- theoretically yes, and this means this way of doing is unsafe, but it's
  -- difficult for a handler to remove itself otherwise.
  Ref.write (Just initialListener) listenerRef
  where
  windowListener :: (WindowId -> Effect Unit) -> SidebarEvent -> Effect Unit
  windowListener callback msg = case msg of
    SbHasWindowId winId -> log ("bg: created winId " <> show winId) *> callback winId
    _ -> pure unit

-- | Initialize the data and the listeners of a new window, and send the current window state.
onNewWindowId ::
  forall a.
  Runtime.Port ->
  (Ref.Ref GlobalState) ->
  (Ref.Ref (Maybe (Listener a))) ->
  WindowId -> Effect Unit
onNewWindowId port stateRef listenerRef winId = do
  -- initial state of the current window
  initWindowState port stateRef winId
  -- remove the previous onMessage listener
  ogListener <- Ref.read listenerRef
  foldMap (\l -> Runtime.onMessageRemoveListener port l) ogListener
  Ref.write Nothing listenerRef
  -- send initial tabs
  latestState <- Ref.read stateRef
  maybe (pure unit)
    ( \w -> do
        Runtime.postMessageJson port
          $ BgInitialTabList
          $ A.fromFoldable
          $ w.positions
          <#> (flip M.lookup w.tabs)
          # A.catMaybes
    )
    (M.lookup winId latestState.windows)
  --  add the new onMessage listener
  sidebarListener <- Runtime.onMessageJsonAddListener port $ manageSidebar stateRef winId port
  onDisconnectListener <- mkListenerUnit $ onDisconnect stateRef winId sidebarListener
  Runtime.portOnDisconnect port onDisconnectListener

-- | Set the port of a new window connecting. If the window doesn't exist, initialize a new data
initWindowState :: Runtime.Port -> (Ref.Ref GlobalState) -> WindowId -> Effect Unit
initWindowState port ref winId =
  (flip Ref.modify_) ref
    $ over (_windows <<< (at winId))
        ( case _ of
            Nothing -> Just $ { tabs: M.empty, port: Just port, positions: empty }
            Just win -> Just $ set _port (Just port) win
        )

-- TODO don't pass the full ref, but only a set of function to manipulate/access 
-- the data required
manageSidebar :: (Ref.Ref GlobalState) -> WindowId -> Runtime.Port -> SidebarEvent -> Effect Unit
manageSidebar ref winId port = case _ of
  SbDeleteTab tabId -> launchAff_ $ removeOne tabId
  SbActivateTab tabId -> launchAff_ $ activateTab tabId
  SbMoveTab tabId newIndex -> moveTab tabId { index: newIndex }
  SbCreateTab tid' -> case tid' of
    Nothing -> createTab { windowId: winId }
    Just tid ->
      Ref.read ref <#> view (_positions >>> _windowIdToWindow winId)
        >>= \positions -> case A.elemIndex tid positions of
            Nothing -> createTab { windowId: winId }
            Just idx -> createTab { windowId: winId, index: idx + 1 }
  _ -> pure unit

onDisconnect :: forall a. (Ref.Ref GlobalState) -> WindowId -> Listener a -> Effect Unit
onDisconnect stateRef winId listener = Ref.modify_ (set (_windows <<< (at winId) <<< _Just <<< _port) Nothing) stateRef
