module PureTabs.Sidebar where

import Browser.Runtime as Runtime
import Browser.Windows (getCurrent)
import Control.Alt (void)
import Control.Alternative (pure)
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Error.Class (throwError)
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude (bind, discard)
import PureTabs.Model (BackgroundEvent(..), SidebarEvent(..))
import PureTabs.Sidebar.Bar as Bar
import PureTabs.Sidebar.Tabs as Tabs
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = do
  port <- Runtime.connect
  HA.runHalogenAff do
    currentWindow <- getCurrent
    liftEffect $ Runtime.postMessageJson port $ SbHasWindowId currentWindow.id
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

onBackgroundMsgConsumer :: (forall a. Tabs.Query a -> Aff (Maybe a)) -> CR.Consumer BackgroundEvent Aff Unit
onBackgroundMsgConsumer query =
  CR.consumer
    $ case _ of
        BgInitialTabList tabs -> do
          void $ query $ H.tell $ Tabs.InitialTabList tabs
          pure Nothing
        BgTabCreated tab -> do
          void $ query $ H.tell $ Tabs.TabCreated tab
          pure Nothing
        BgTabDeleted tabId -> do
          void $ query $ H.tell $ Tabs.TabDeleted tabId
          pure Nothing
        BgTabActivated prev next -> do
          void $ query $ H.tell $ Tabs.TabActivated prev next
          pure Nothing
        BgTabMoved tabId prev next -> do
          void $ query $ H.tell $ Tabs.TabMoved tabId prev next
          pure Nothing
        BgTabUpdated tabId cinfo tab -> do
          void $ query $ H.tell $ Tabs.TabInfoChanged tabId cinfo
          pure Nothing
        _ -> pure Nothing

onSidebarMsg :: Runtime.Port -> CR.Consumer SidebarEvent Aff Unit
onSidebarMsg port =
  CR.consumer \(msg) -> do
    liftEffect $ Runtime.postMessageJson port msg
    pure Nothing
