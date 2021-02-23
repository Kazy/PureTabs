module PureTabs.Sidebar where

import Browser.Runtime as Runtime
import Browser.Tabs (Tab(..), WindowId)
import Browser.Tabs.OnUpdated (ChangeInfo(..))
import Browser.Windows (getCurrent)
import Control.Alt (void)
import Control.Alternative (pure)
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Error.Class (throwError, try)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Time.Duration (Milliseconds(..))
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Aff (Aff, delay, error)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Console as Log
import Effect.Exception (message)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude (bind, discard, (*), (-), (<>))
import PureTabs.Model.BackgroundEvent (BackgroundEvent(..))
import PureTabs.Model.SidebarEvent (SidebarEvent(..))
import PureTabs.Sidebar.Bar as Bar
import PureTabs.Sidebar.Tabs as Tabs
import Web.DOM.ParentNode (QuerySelector(..))


-- | Try to connect the port and to send the first message, retrying multiple
-- | times if it does not succeed.
trySendWindowId :: WindowId -> Aff Runtime.Port
trySendWindowId windowId = loopConnect 5 (Milliseconds 50.0)
  where 
        tryConnect = do 
            port <- Runtime.connect 
            Runtime.postMessageJson port (SbHasWindowId windowId) 
            log "[sb] windowId sent"
            pure port

        loopConnect :: Int -> Milliseconds -> Aff Runtime.Port
        loopConnect 0 _ = 
          throwError $ error "[sb] couldn't connect to the background extesion :("
        loopConnect attemptLeft timeout = do
          liftEffect $ 
            log $ "[sb] attempt to connect to background extension (left: " <> (show attemptLeft) <> ")"
          success <- try $ liftEffect tryConnect
          case success of 
               Left err -> do
                 liftEffect $ Log.error $ message err
                 delay timeout 
                 loopConnect (attemptLeft - 1) (multiplyMs 2.0 timeout)
               Right port -> pure port

        multiplyMs by (Milliseconds t) = Milliseconds (t * by)



main :: Effect Unit
main = do
  log "[sb] starting"
  HA.runHalogenAff do
    currentWindow <- getCurrent
    port <- trySendWindowId currentWindow.id
    content' <- HA.selectElement (QuerySelector "#content")
    io <- case content' of
      Nothing -> throwError (error "Could not find #content")
      Just content -> runUI Bar.component unit content
    io.subscribe $ onSidebarMsg port
    CR.runProcess ((onBackgroundMsgProducer port) CR.$$ onBackgroundMsgConsumer io.query)

onBackgroundMsgProducer :: Runtime.Port -> CR.Producer BackgroundEvent Aff Unit
onBackgroundMsgProducer port =
  CRA.produce \emitter ->
    liftEffect $ void $ Runtime.onMessageJsonAddListener port (emit emitter)


onBackgroundMsgConsumer :: (forall a. Bar.Query a -> Aff (Maybe a)) -> CR.Consumer BackgroundEvent Aff Unit
onBackgroundMsgConsumer query =
  CR.consumer
    $ case _ of

        BgInitialTabList groups tabs -> do
          void $ query $ H.tell $ \q -> Bar.InitialTabsWithGroup groups tabs q 
          pure Nothing

        BgTabCreated tab -> do
          void $ query $ H.tell $ \q -> Bar.TabsQuery (Tabs.TabCreated tab q)
          pure Nothing

        BgTabDeleted tabId -> do
          void $ query $ H.request $ \q -> Bar.TabsQuery (Tabs.TabDeleted tabId q)
          pure Nothing

        BgTabActivated prev next -> do
          void $ query $ H.tell $ \q -> Bar.TabsQuery (Tabs.TabActivated prev next q)
          pure Nothing

        BgTabMoved tabId prev next -> do
          void $ query $ H.tell $ \q -> Bar.TabsQuery (Tabs.TabMoved tabId next q)
          pure Nothing

        BgTabUpdated tabId cinfo tab -> do
          void $ query $ H.tell $ \q -> Bar.TabsQuery (Tabs.TabInfoChanged tabId (fillChangeInfoIfEmpty tab cinfo) q)
          pure Nothing

        BgTabDetached tabId -> do 
          void $ query $ H.tell $ \q -> Bar.TabsQuery (Tabs.TabDetached tabId q)
          pure Nothing

        BgTabAttached tab -> do 
          void $ query $ H.tell $ \q -> Bar.TabsQuery (Tabs.TabAttached tab q)
          pure Nothing

        BgGroupDeleted gid currentTid -> do
           void $ query $ H.tell $ Bar.GroupDeleted gid currentTid
           pure Nothing

-- | Workaround for https://bugzilla.mozilla.org/show_bug.cgi?id=1640112
-- | In case the ChangeInfo only contains an update for the status field to "complete",
-- | we generate a new one with all the information from the tab.
-- | It seems necessary because when restoring a session, the first tab we're in
-- | won't get the proper tab information in the ChangeInfo structure, and will be stuck
-- | with the "Restore Session" title and its corresponding favicon.
fillChangeInfoIfEmpty :: Tab -> ChangeInfo -> ChangeInfo
fillChangeInfoIfEmpty (Tab tab) = 
  case _ of 
      ChangeInfo { attention: Nothing
                 , audible: Nothing
                 , discarded: Nothing
                 , favIconUrl: Nothing
                 , hidden: Nothing
                 , isArticle: Nothing
                 , pinned: Nothing
                 , status: Just "complete"
                 , title: Nothing
                 , url: Nothing 
                 } ->
            ChangeInfo { attention: tab.attention
            , audible: tab.audible
            , discarded: tab.discarded
            , favIconUrl: tab.favIconUrl
            , hidden: Just tab.hidden
            , isArticle: tab.isArticle
            , pinned: Just tab.pinned
            , status: Just "complete"
            , title: Just tab.title
            , url: tab.url
            }
      cinfo -> cinfo

onSidebarMsg :: Runtime.Port -> CR.Consumer SidebarEvent Aff Unit
onSidebarMsg port =
  CR.consumer \(msg) -> do
    liftEffect $ Runtime.postMessageJson port msg
    pure Nothing
