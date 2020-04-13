module PureTabs.Sidebar where

import Browser.Runtime as Runtime
import Browser.Tabs (Tab(..), TabId(..), WindowId)
import Browser.Utils (mkListenerOne)
import Browser.Windows (getCurrent)
import Control.Alternative (pure)
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.Unit (unit)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import JQuery (JQuery, append, create, find, remove, select, setAttr, setText)
import Prelude (Unit, bind, ($), discard)
import PureTabs.Model (BackgroundEvent(..), SidebarEvent(..))

main :: Effect Unit
main = do
  log "started sidebar"
  port <- Runtime.connect
  launchAff_ $ runSidebar port
  where
  runSidebar :: Runtime.Port -> Aff Unit
  runSidebar port = do
    currentWindow <- getCurrent
    liftEffect $ initSidebar port currentWindow.id

initSidebar :: Runtime.Port -> WindowId -> Effect Unit
initSidebar port winId = do
  log $ "windowId " <> (show winId)
  Runtime.postMessageJson port $ SbHasWindowId winId
  content <- select "#content"
  _ <- Runtime.onMessageJsonAddListener port $ onMsg content
  pure unit
  where
  onMsg :: JQuery -> BackgroundEvent -> Effect Unit
  onMsg contentDiv event = case event of
    BgTabCreated tab -> do
      tabElem <- createTabElement tab
      append tabElem contentDiv
      pure unit
    BgTabDeleted tabId -> deleteTabElement tabId
    _ -> log "received unsupported message type"

createTabElement :: Tab -> Effect JQuery
createTabElement tab' = do
  let
    tab = unwrap tab'
  div <- create "<div>"
  setText tab.title div
  setAttr "id" tab.id div
  pure div

deleteTabElement :: TabId -> Effect Unit
deleteTabElement tabId = do
    div <- select ("#" <> show tabId)
    remove div
