module PureTabs.Background where

import Data.List
import Browser.Runtime as Runtime
import Browser.Tabs (Tab, TabId(..), WindowId)
import Browser.Tabs.OnCreated as TabsOnCreated
import Browser.Tabs.OnRemoved as TabsOnRemoved
import Browser.Utils (Listener, mkListenerOne, mkListenerTwo, mkListenerUnit)
import Control.Alt (map)
import Control.Alternative (pure, (*>))
import Data.Foldable (for_)
import Data.Function (flip)
import Data.Lens (_Just, over, preview, set, view)
import Data.Lens.At (at)
import Data.Map (empty)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.Unit (unit)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import Prelude (Unit, bind, ($), discard, (<<<))
import PureTabs.Model (_windows, _portFromWindow, _tabFromWindow, _port, _tabFromTabIdAndWindow, initialGlobalState, GlobalState, BackgroundEvent(..), SidebarEvent(..))

type Ports
  = Ref.Ref (List Runtime.Port)

main :: Effect Unit
main = do
  log "starting background"
  state <- Ref.new initialGlobalState
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
onConnect stateRef port = do
  listenerRef <- Ref.new Nothing
  initialListener <- Runtime.onMessageJsonAddListener port $ windowListener $ onNewWindowId listenerRef
  Ref.write (Just initialListener) listenerRef
  where
  windowListener :: (WindowId -> Effect Unit) -> SidebarEvent -> Effect Unit
  windowListener callback msg = case msg of
    SbHasWindowId winId -> log ("bg: created winId " <> show winId) *> callback winId
    _ -> pure unit

  onNewWindowId :: forall a. (Ref.Ref (Maybe (Listener a))) -> WindowId -> Effect Unit
  onNewWindowId listenerRef winId =
    let
      winLens = _windows <<< (at winId)
    in
      do
        (flip Ref.modify_) stateRef
          $ over winLens
              ( case _ of
                  Nothing -> Just $ { tabs: empty, port: Just port }
                  Just win -> Just $ set _port (Just port) win
              )
        r <- Ref.read stateRef
        ogListener <- Ref.read listenerRef
        foldMap (\l -> Runtime.onMessageRemoveListener port l) ogListener
        Ref.write Nothing listenerRef
        sidebarListener <- Runtime.onMessageJsonAddListener port $ manageSidebar stateRef port
        onDisconnectListener <- mkListenerUnit $ onDisconnect stateRef winId sidebarListener
        Runtime.portOnDisconnect port onDisconnectListener

-- TODO don't pass the full ref, but only a set of function to manipulate/access 
-- the data required
manageSidebar :: (Ref.Ref GlobalState) -> Runtime.Port -> SidebarEvent -> Effect Unit
manageSidebar stateRef port msg = do
  pure unit

onDisconnect :: forall a. (Ref.Ref GlobalState) -> WindowId -> Listener a -> Effect Unit
onDisconnect stateRef winId listener = Ref.modify_ (set (_windows <<< (at winId) <<< _Just <<< _port) Nothing) stateRef
