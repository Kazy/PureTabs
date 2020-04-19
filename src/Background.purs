module PureTabs.Background where

import Browser.Runtime as Runtime
import Browser.Tabs (Tab, TabId, WindowId, query)
import Browser.Tabs.OnCreated as TabsOnCreated
import Browser.Tabs.OnRemoved as TabsOnRemoved
import Browser.Utils (Listener, mkListenerOne, mkListenerTwo, mkListenerUnit)
import Control.Alt (map)
import Control.Alternative (pure, (*>))
import Data.Array (fromFoldable)
import Data.Foldable (for_)
import Data.Function (flip)
import Data.Lens (_Just, over, preview, set)
import Data.Lens.At (at)
import Data.List (List, foldr, foldMap)
import Data.Map (empty, lookup, values)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.Unit (unit)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import Prelude (Unit, bind, ($), discard, (<<<))
import PureTabs.Model (_windows, _portFromWindow, _tabFromWindow, _port, _tabFromTabIdAndWindow, tabsToGlobalState, GlobalState, BackgroundEvent(..), SidebarEvent(..))

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
    liftEffect
      $ do
          state <- Ref.new $ tabsToGlobalState allTabs
          initializeBackground state
          log "all listener initialized"

initializeBackground :: Ref.Ref GlobalState -> Effect Unit
initializeBackground ref = do
  _ <- TabsOnCreated.addListener $ onTabCreated ref
  tabDeletedListener <- mkListenerTwo $ onTabDeleted ref
  _ <- TabsOnRemoved.addListener tabDeletedListener
  onConnectedListener <- mkListenerOne $ onConnect ref
  Runtime.onConnectAddListener onConnectedListener
  pure unit

-- port on connect
-- created tab
onTabCreated :: (Ref.Ref GlobalState) -> Tab -> Effect Unit
onTabCreated stateRef tab' = do
  state <- Ref.modify (set (_tabFromWindow tab') (Just tab')) stateRef
  log $ "tabId: " <> (show tab.id) <> " windowId " <> show tab.windowId
  case (preview (_portFromWindow tab') state) of
    Nothing -> pure unit
    Just port -> do
      _ <- Runtime.postMessageJson port $ BgTabCreated tab'
      log $ "tab " <> (show tab.id) <> " created: " <> tab.title
  where
  tab = unwrap tab'

onTabDeleted :: (Ref.Ref GlobalState) -> TabId -> TabsOnRemoved.RemoveInfo -> Effect Unit
onTabDeleted stateRef tabId info = do
  state <- Ref.read stateRef
  let
    allTabs = _tabFromTabIdAndWindow state tabId

    newState = foldr (\t -> set (_tabFromWindow t) Nothing) state allTabs
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

onNewWindowId ::
  forall a.
  Runtime.Port ->
  (Ref.Ref GlobalState) ->
  ( Ref.Ref
      ( Maybe
          (Listener a)
      )
  ) ->
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
    (\w -> Runtime.postMessageJson port $ BgInitialTabList $ fromFoldable $ values w.tabs)
    (lookup winId r.windows)
  --  add the new onMessage listener
  sidebarListener <- Runtime.onMessageJsonAddListener port $ manageSidebar stateRef port
  onDisconnectListener <- mkListenerUnit $ onDisconnect stateRef winId sidebarListener
  Runtime.portOnDisconnect port onDisconnectListener

initWindowState :: Runtime.Port -> (Ref.Ref GlobalState) -> WindowId -> Effect GlobalState
initWindowState port ref winId =
  (flip Ref.modify) ref
    $ over (_windows <<< (at winId))
        ( case _ of
            Nothing -> Just $ { tabs: empty, port: Just port }
            Just win -> Just $ set _port (Just port) win
        )

-- TODO don't pass the full ref, but only a set of function to manipulate/access 
-- the data required
manageSidebar :: (Ref.Ref GlobalState) -> Runtime.Port -> SidebarEvent -> Effect Unit
manageSidebar stateRef port msg = do
  pure unit

onDisconnect :: forall a. (Ref.Ref GlobalState) -> WindowId -> Listener a -> Effect Unit
onDisconnect stateRef winId listener = Ref.modify_ (set (_windows <<< (at winId) <<< _Just <<< _port) Nothing) stateRef
