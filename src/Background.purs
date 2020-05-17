module PureTabs.Background where

import Browser.Runtime as Runtime
import Browser.Tabs (Tab(..), TabId, WindowId, query, removeOne, activateTab)
import Browser.Tabs.OnActivated as OnActivated
import Browser.Tabs.OnCreated as OnCreated
import Browser.Tabs.OnMoved as OnMoved
import Browser.Tabs.OnRemoved as OnRemoved
import Browser.Tabs.OnUpdated as OnUpdated
import Browser.Utils (Listener, mkListenerOne, mkListenerTwo, mkListenerUnit)
import Control.Alt ((<#>))
import Control.Alternative (empty, pure, (*>))
import Control.Bind ((=<<), (>>=))
import Control.Category (identity, (>>>))
import Data.Array (catMaybes, deleteAt, foldl, fromFoldable, insertAt, mapWithIndex, (!!))
import Data.Foldable (for_)
import Data.Function (flip, (#))
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
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref as Ref
import Prelude (Unit, bind, ($), discard, (<<<))
import PureTabs.Model (BackgroundEvent(..), GlobalState, SidebarEvent(..), _active, _index, _port, _portFromWindow, _portFromWindowId, _positions, _tabFromTabIdAndWindow, _tabFromWindow, _tabs, _windowIdToWindow, _windows, tabsToGlobalState)

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
  OnCreated.addListener $ onTabCreated ref
  (mkListenerTwo $ onTabDeleted ref) >>= OnRemoved.addListener
  OnActivated.addListener $ onTabActived ref
  OnUpdated.addListener $ onTabUpdated ref
  (mkListenerTwo $ onTabMoved ref) >>= OnMoved.addListener
  (mkListenerOne $ onConnect ref) >>= Runtime.onConnectAddListener

onTabCreated :: (Ref.Ref GlobalState) -> Tab -> Effect Unit
onTabCreated stateRef tab' = do
  state <-
    Ref.modify
      ( set (_tabFromWindow tab') (Just tab')
          *> over (_positions >>> _windowIdToWindow tab.windowId)
          -- TODO: throw an error here instead. Encapsulate the manipulations of
          -- the position array to make sure we always perform valid operation
          -- and otherwise throw an error or recover from it.
              (\p -> maybe p identity (insertAt tab.index tab.id p))
      )
      stateRef
  log $ "tabId: " <> (show tab.id) <> " windowId " <> show tab.windowId
  case (preview (_portFromWindow tab') state) of
    Nothing -> pure unit
    Just port -> do
      _ <- Runtime.postMessageJson port $ BgTabCreated tab'
      log $ "tab " <> (show tab.id) <> " created: " <> tab.title
  where
  tab = unwrap tab'

onTabUpdated :: (Ref.Ref GlobalState) -> TabId -> OnUpdated.ChangeInfo -> Tab -> Effect Unit
onTabUpdated stateRef tid cinfo tab' = do
  state <- Ref.modify (set (_tabFromWindow tab') (Just tab')) stateRef
  case (preview (_portFromWindow tab') state) of
    Nothing -> pure unit
    Just port -> Runtime.postMessageJson port $ BgTabUpdated tid cinfo tab'

onTabMoved :: (Ref.Ref GlobalState) -> TabId -> OnMoved.MoveInfo -> Effect Unit
onTabMoved ref tid minfo = do
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

  updateTabsIndex :: Array TabId -> M.Map TabId Tab -> M.Map TabId Tab
  updateTabsIndex positions tabs =
    let
      modifyFuncs :: Array (M.Map TabId Tab -> M.Map TabId Tab)
      modifyFuncs = mapWithIndex (\idx tid' -> set (at tid' <<< _Just <<< _Newtype <<< _index) idx) positions
    in
      foldl (#) tabs modifyFuncs

  unsafeUpdatePositions :: OnMoved.MoveInfo -> Array TabId -> Array TabId
  unsafeUpdatePositions minfo' =
    (moveElement minfo'.fromIndex minfo'.toIndex)
      -- the indexes should exist, we need to revisit the code if it doesn't
      
      >>> (maybe' (\_ -> unsafeThrow "invalid indexes") identity)

  moveElement :: forall a. Int -> Int -> Array a -> Maybe (Array a)
  moveElement from to arr = do
    tab <- arr !! from
    deleteAt from arr >>= insertAt to tab

onTabActived :: (Ref.Ref GlobalState) -> OnActivated.ActiveInfo -> Effect Unit
onTabActived stateRef (OnActivated.ActiveInfo aInfo) = do
  log $ "activated " <> show aInfo.tabId
  state <- Ref.modify (updateGlobalState aInfo.previousTabId aInfo.tabId) stateRef
  case (preview (_portFromWindowId aInfo.windowId) state) of
    Nothing -> pure unit
    Just port -> Runtime.postMessageJson port $ BgTabActived aInfo.previousTabId aInfo.tabId
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

onTabDeleted :: (Ref.Ref GlobalState) -> TabId -> OnRemoved.RemoveInfo -> Effect Unit
onTabDeleted stateRef tabId info = do
  state <- Ref.read stateRef
  let
    allTabs = _tabFromTabIdAndWindow state tabId

    deleteTabState :: Tab -> GlobalState -> GlobalState
    deleteTabState t = set (_tabFromWindow t) Nothing

    deletePositionState :: Tab -> GlobalState -> GlobalState
    deletePositionState (Tab t) = over 
      (_positions >>> _windowIdToWindow t.windowId)
      (\p -> maybe p identity (deleteAt t.index p))

    newState = foldr (\t -> deleteTabState t >>> deletePositionState t) state allTabs
  Ref.write newState stateRef
  for_ allTabs \t -> do
    let
      port = preview (_portFromWindow t) state
    maybe (pure unit) ((flip Runtime.postMessageJson) (BgTabDeleted tabId)) port

onConnect :: (Ref.Ref GlobalState) -> Runtime.Port -> Effect Unit
onConnect stateRef' port = do
  -- create a temporary listener ref that will only be held until the sidebar has sent its current window
  listenerRef <- Ref.new Nothing
  initialListener <-
    Runtime.onMessageJsonAddListener port $ windowListener
      $ onNewWindowId port stateRef' listenerRef
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
  r <- initWindowState port stateRef winId
  -- remove the previous onMessage listener
  ogListener <- Ref.read listenerRef
  foldMap (\l -> Runtime.onMessageRemoveListener port l) ogListener
  Ref.write Nothing listenerRef
  -- send initial tabs
  maybe (pure unit)
    ( \w ->
        Runtime.postMessageJson port
          $ BgInitialTabList
          $ fromFoldable
          $ w.positions
          <#> (flip M.lookup w.tabs)
          # catMaybes
    )
    (M.lookup winId r.windows)
  --  add the new onMessage listener
  sidebarListener <- Runtime.onMessageJsonAddListener port $ manageSidebar stateRef port
  onDisconnectListener <- mkListenerUnit $ onDisconnect stateRef winId sidebarListener
  Runtime.portOnDisconnect port onDisconnectListener

-- | Set the port of a new window connecting. If the window doesn't exist, initialize a new data
initWindowState :: Runtime.Port -> (Ref.Ref GlobalState) -> WindowId -> Effect GlobalState
initWindowState port ref winId =
  (flip Ref.modify) ref
    $ over (_windows <<< (at winId))
        ( case _ of
            Nothing -> Just $ { tabs: M.empty, port: Just port, positions: empty }
            Just win -> Just $ set _port (Just port) win
        )

-- TODO don't pass the full ref, but only a set of function to manipulate/access 
-- the data required
manageSidebar :: (Ref.Ref GlobalState) -> Runtime.Port -> SidebarEvent -> Effect Unit
manageSidebar stateRef port (SbTabDeleted tabId) = launchAff_ $ removeOne tabId

manageSidebar stateRef port (SbTabActived tabId) = launchAff_ $ activateTab tabId

manageSidebar stateRef port msg = pure unit

onDisconnect :: forall a. (Ref.Ref GlobalState) -> WindowId -> Listener a -> Effect Unit
onDisconnect stateRef winId listener = Ref.modify_ (set (_windows <<< (at winId) <<< _Just <<< _port) Nothing) stateRef
