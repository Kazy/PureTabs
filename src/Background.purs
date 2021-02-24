module PureTabs.Background where

import Browser.Runtime as Runtime
import Browser.Tabs (Tab(..), TabId, WindowId)
import Browser.Tabs as BT
import Browser.Tabs.OnActivated as OnActivated
import Browser.Tabs.OnAttached as OnAttached
import Browser.Tabs.OnCreated as OnCreated
import Browser.Tabs.OnDetached as OnDetached
import Browser.Tabs.OnMoved as OnMoved
import Browser.Tabs.OnRemoved as OnRemoved
import Browser.Tabs.OnUpdated as OnUpdated
import Browser.Utils (Listener, mkListenerOne, mkListenerTwo, mkListenerUnit, unsafeLog)
import Browser.Windows (Window)
import Browser.Windows.OnCreated as WinOnCreated
import Browser.Windows.OnRemoved as WinOnRemoved
import Control.Alt ((<#>))
import Control.Alternative ((*>))
import Control.Bind ((=<<), (>>=))
import Control.Category ((>>>))
import Data.Array as A
import Data.CommutativeRing ((+))
import Data.Function (flip, (#))
import Data.Lens (_Just, set, view)
import Data.Lens.At (at)
import Data.List (List, foldMap)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Show (show)
import Data.Traversable (traverse)
import Data.Unit (unit)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import Prelude (Unit, bind, discard, pure, ($), (<$>), (<<<))
import PureTabs.Browser.Sessions (getTabValue, removeTabValue, setTabValue)
import PureTabs.Model.BackgroundEvent (BackgroundEvent(..))
import PureTabs.Model.GlobalState as GS
import PureTabs.Model.Group (GroupId(..))
import PureTabs.Model.GroupMapping (GroupData, createGroup, deleteGroup, moveGroup, renameGroup, retrieveGroups, updateGroupsMapping)
import PureTabs.Model.SidebarEvent (SidebarEvent(..))
import PureTabs.Model.TabWithGroup (TabWithGroup(..))

type Ports
  = Ref.Ref (List Runtime.Port)

type StateRef = Ref.Ref GS.GlobalState


main :: Effect Unit
main = do
  log "[bg] starting"
  launchAff_ do
     allTabs <- BT.browserQuery {}
     liftEffect $ initializeBackground =<< (Ref.new $ GS.initialTabsToGlobalState allTabs)

initializeBackground :: Ref.Ref GS.GlobalState -> Effect Unit
initializeBackground ref = do
  (mkListenerOne $ onConnect ref) >>= Runtime.onConnectAddListener
  (mkListenerOne $ onWindowCreated ref) >>= WinOnCreated.addListener
  (mkListenerOne $ onWindowRemoved ref) >>= WinOnRemoved.addListener
  onTabCreated ref # OnCreated.addListener
  (mkListenerTwo $ onTabDeleted ref) >>= OnRemoved.addListener
  onTabActived ref # OnActivated.addListener
  onTabUpdated ref # OnUpdated.addListener
  (mkListenerTwo $ onTabDetached ref) >>= OnDetached.addListener
  (mkListenerTwo $ onTabAttached ref) >>= OnAttached.addListener
  (mkListenerTwo $ onTabMoved ref) >>= OnMoved.addListener

onWindowCreated :: StateRef -> Window -> Effect Unit
onWindowCreated ref { id: winId } = do
  log $ "bg: created window " <> (show winId)
  ref # Ref.modify_ (GS.addEmptyWindow winId)

onWindowRemoved :: StateRef -> WindowId -> Effect Unit
onWindowRemoved ref winId = do
  log $ "bg: deleted window " <> (show winId)
  ref # Ref.modify_ (GS.deleteWindow winId)

onTabCreated :: StateRef -> Tab -> Effect Unit
onTabCreated stateRef tab = do
  log $ "bg: created tab " <> (BT.showTabId tab) 
  state <- Ref.modify (GS.createTab tab) stateRef

  let Tab(t) = tab

  -- Attempt to detect session restore.
  -- If the tab we're opening already has a `groupId` value, it is either a
  -- restored tab from the current session, or a restored tab from a full
  -- session restore. If we found groups associated with the tab's window, we
  -- ask the sidebar to initiliaze them.
  -- 
  -- This solution ignores one use case (for which it will probably be buggy):
  -- opening a session on top of an already existing session. If the user
  -- starts creating groups, opening tab, and then restore a session, then it
  -- will probably break.

  -- An other issue with this solution is that the action is triggered in a
  -- fiber, delaying its action, and in particular allowing the tab creation
  -- event to be sent later than e.g. the tab activation event to the sidebar.
  -- A possible fix could be send the TabCreated event just as before, and then
  -- launch a fiber to ask the sidebar to switch the tab's group (and
  -- initialize the groups) in case it's needed.
  launchAff_ $
     (getTabValue t.id "groupId" :: Aff (Maybe GroupId)) >>= 
       case _ of
            Nothing -> liftEffect $ GS.sendToTabPort tab state $ BgTabCreated tab Nothing
            Just gid -> do 
               retrieveGroups t.windowId >>= 
                 case _ of 
                      [] -> pure unit
                      groups -> liftEffect $ GS.sendToTabPort tab state $ BgInitializeGroups groups
               liftEffect $ GS.sendToTabPort tab state $ BgTabCreated tab (Just gid)
               

onTabUpdated :: StateRef -> TabId -> OnUpdated.ChangeInfo -> Tab -> Effect Unit
onTabUpdated stateRef tid cinfo tab = do
  log $ "bg: updated tab " <> show tid
  state <- Ref.modify (GS.updateTab tab) stateRef
  GS.sendToTabPort tab state $ BgTabUpdated tid cinfo tab

onTabMoved :: StateRef -> TabId -> OnMoved.MoveInfo -> Effect Unit
onTabMoved ref tid minfo = do
  log $ "bg: moved tab " <> show tid
  state <- Ref.modify (GS.moveTab minfo.fromIndex minfo.toIndex minfo.windowId) ref
  GS.sendToWindowPort minfo.windowId state $ BgTabMoved tid minfo.fromIndex minfo.toIndex

onTabActived :: StateRef -> OnActivated.ActiveInfo -> Effect Unit
onTabActived stateRef (OnActivated.ActiveInfo aInfo) = do
  log $ "bg: activated tab " <> show aInfo.tabId
  state <- Ref.modify (GS.activateTab aInfo.windowId aInfo.previousTabId aInfo.tabId) stateRef
  GS.sendToWindowPort aInfo.windowId state $ BgTabActivated aInfo.previousTabId aInfo.tabId

onTabDeleted :: StateRef -> TabId -> OnRemoved.RemoveInfo -> Effect Unit
onTabDeleted stateRef tabId info = do 
  log $ "bg: deleted tab " <> show tabId
  state <- Ref.modify (GS.deleteTab info.windowId tabId) stateRef
  GS.sendToWindowPort info.windowId state $ BgTabDeleted tabId

onTabDetached :: StateRef -> TabId -> OnDetached.DetachInfo -> Effect Unit
onTabDetached stateRef tabId info = do
  log $ "bg: detached tab " <> show tabId
  state <- Ref.modify (GS.detachTab info.oldWindowId tabId) stateRef
  GS.sendToWindowPort info.oldWindowId state $ BgTabDetached tabId

onTabAttached :: StateRef -> TabId -> OnAttached.AttachInfo -> Effect Unit
onTabAttached stateRef tid info = do
  log $ "bg: attached tab " <> show tid
  state <- Ref.modify (GS.attachTab info.newWindowId tid info.newPosition) stateRef
  case GS.tabFromWinIdAndTabId info.newWindowId tid state of
     Just newTab -> GS.sendToWindowPort info.newWindowId state $ BgTabAttached newTab
     Nothing -> pure unit

onConnect :: StateRef -> Runtime.Port -> Effect Unit
onConnect stateRef port = do
  log "[bg] connection received"
  -- Create a temporary listener ref that will only be held until the sidebar has sent its current window
  listenerRef <- Ref.new Nothing
  initialListener <-
    Runtime.onMessageJsonAddListener port $ windowListener
      $ onNewWindowId port stateRef listenerRef
  -- XXX: Is it possible a message arrives *before* this is executed ? 
  -- Theoretically yes, and this means this way of doing is unsafe, but it's
  -- difficult for a handler to remove itself otherwise.
  Ref.write (Just initialListener) listenerRef

  where

  windowListener :: (WindowId -> Effect Unit) -> SidebarEvent -> Effect Unit
  windowListener callback msg = case msg of
    SbHasWindowId winId -> log ("[bg] created winId " <> show winId) *> callback winId
    _ -> pure unit

-- | Initialize the data and the listeners of a new window, and send the current window state.
onNewWindowId 
  :: forall a. 
   Runtime.Port 
  -> StateRef 
  -> (Ref.Ref (Maybe (Listener a)))
  -> WindowId 
  -> Effect Unit
onNewWindowId port stateRef listenerRef winId = do
  -- Initial state of the current window
  Ref.modify_ (GS.initializeWindowState winId port) stateRef

  -- Remove the previous onMessage listener
  ogListener <- Ref.read listenerRef
  foldMap (\l -> Runtime.onMessageRemoveListener port l) ogListener
  Ref.write Nothing listenerRef

  -- Send initial tabs
  latestState <- Ref.read stateRef
  M.lookup winId latestState.windows # foldMap \w ->
    let 
        tabs = A.fromFoldable
          $ w.positions
          <#> (flip M.lookup w.tabs)
          # A.catMaybes

    in
      launchAff_ do
         groups <- initialWindowGroups
         tabsWithGroup <- initialTabsGroups tabs groups
         liftEffect $ Runtime.postMessageJson port $ BgInitialTabList groups tabsWithGroup
    
  --  Add the new onMessage listener
  sidebarListener <- Runtime.onMessageJsonAddListener port $ manageSidebar stateRef winId port
  onDisconnectListener <- mkListenerUnit $ onDisconnect stateRef winId sidebarListener
  Runtime.portOnDisconnect port onDisconnectListener

  where
        -- | Set a default group if none exist.
        initialWindowGroups :: Aff (Array GroupData)
        initialWindowGroups = 
           retrieveGroups winId >>= 
             case _ of 
                  [] -> updateGroupsMapping winId (createGroup (GroupId 0) "main") 
                      *> retrieveGroups winId >>= \groups' -> pure groups'
                  groups' -> pure groups'

        -- | For each tab, set a default tab if it doesn't exist
        initialTabsGroups :: Array Tab -> Array GroupData -> Aff (Array TabWithGroup)
        initialTabsGroups tabs groups = 
          let 
              defaultGroup = groups # (A.head >>> maybe (GroupId 0) (unwrap >>> _.groupId))
          in
              tabs # traverse \tab@(Tab t) -> (getTabValue t.id "groupId" :: Aff (Maybe GroupId)) >>= 
                case _ of 
                     Nothing -> setTabValue t.id "groupId" defaultGroup *> pure (TabWithGroup tab defaultGroup)
                     Just gid -> pure $ TabWithGroup tab gid



manageSidebar :: StateRef -> WindowId -> Runtime.Port -> SidebarEvent -> Effect Unit
manageSidebar ref winId port = case _ of

  SbDeleteTab tabId -> launchAff_ $ BT.browserRemoveOne tabId

  SbActivateTab tabId -> launchAff_ $ BT.browserActivateTab tabId

  SbMoveTab tabId newIndex -> BT.browserMoveTab tabId { index: newIndex }

  SbCreateTab tid' -> case tid' of
    Nothing -> BT.browserCreateTab { windowId: winId }
    Just tid ->
      Ref.read ref <#> view (GS._positions >>> GS._windowIdToWindow winId)
        >>= \positions -> case A.elemIndex tid positions of
            Nothing -> BT.browserCreateTab { windowId: winId }
            Just idx -> BT.browserCreateTab { windowId: winId, index: idx + 1 }

  SbSelectedGroup tabIds -> do
     state <- Ref.read ref
     let 
         allTabIds = M.keys $ view ((GS._windowIdToWindow winId) <<< GS._tabs) state
         tabIdsToHide = A.fromFoldable $ Set.difference allTabIds (Set.fromFoldable tabIds)

     BT.browserHideTabs tabIdsToHide
     unsafeLog tabIdsToHide
     BT.browserShowTabs tabIds
     unsafeLog tabIds

  SbDeletedGroup gid tabIds -> launchAff_ do
     BT.browserRemove tabIds
     activeTab <- BT.browserQuery { windowId: unwrap winId, active: true }
     let activeTabId = activeTab # A.head >>> (<$>) (unwrap >>> _.id)
     liftEffect $ Runtime.postMessageJson port $ BgGroupDeleted gid activeTabId
     updateGroupsMapping winId $ deleteGroup gid

  SbChangeTabGroup tid Nothing -> launchAff_ $ removeTabValue tid "groupId"
  SbChangeTabGroup tid (Just gid) -> launchAff_ $ setTabValue tid "groupId" gid

  SbCreatedGroup gid name -> launchAff_ $ updateGroupsMapping winId $ createGroup gid name
  SbRenamedGroup gid name -> launchAff_ $ updateGroupsMapping winId $ renameGroup gid name
  SbMovedGroup gid pos -> launchAff_ $ updateGroupsMapping winId $ moveGroup gid pos

  SbDetacheTab -> pure unit
  SbHasWindowId winId' -> pure unit


onDisconnect :: forall a. StateRef -> WindowId -> Listener a -> Effect Unit
onDisconnect stateRef winId listener = Ref.modify_ (set (GS._windows <<< (at winId) <<< _Just <<< GS._port) Nothing) stateRef
