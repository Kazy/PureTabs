module PureTabs.Sidebar where

import Browser.Runtime as Runtime
import Browser.Tabs (Tab, TabId, WindowId)
import Browser.Tabs.OnUpdated (ChangeInfo(..))
import Browser.Windows (getCurrent)
import Control.Alternative (pure)
import Control.Bind ((>=>))
import Data.Foldable (traverse_)
import Data.Function (flip)
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.Unit (unit)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import JQuery (JQuery, append, create, remove, select, setAttr, setText)
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
  tabsDiv <- select "#tabs"
  _ <- Runtime.onMessageJsonAddListener port $ onMsg tabsDiv
  pure unit
  where
  onMsg :: JQuery -> BackgroundEvent -> Effect Unit
  onMsg contentDiv event = case event of
    BgTabCreated tab -> do
      tabElem <- createTabElement tab
      append tabElem contentDiv
      pure unit
    BgTabDeleted tabId -> deleteTabElement tabId
    BgInitialTabList tabs -> traverse_ (createTabElement >=> (flip append) contentDiv) tabs
    BgTabUpdated tid cinfo tab -> updateTabInfo tid cinfo tab
    _ -> log "received unsupported message type"

createTabElement :: Tab -> Effect JQuery
createTabElement tab' = do
  let
    tab = unwrap tab'
  tabDiv <- create "<div>"
  setAttr "class" "tab" tabDiv
  setAttr "id" tab.id tabDiv
  favicon <- create "<span class=\"favicon\">"
  tabTitle <- create "<div class=\"tab-title\">"
  setText tab.title tabTitle
  append favicon tabDiv
  append tabTitle tabDiv
  pure tabDiv

deleteTabElement :: TabId -> Effect Unit
deleteTabElement tabId = do
  div <- select ("#" <> show tabId)
  remove div

updateTabInfo :: TabId -> ChangeInfo -> Tab -> Effect Unit
updateTabInfo tid cinfo' tab' = do
  let
    tab = unwrap tab'

    cinfo = unwrap cinfo'
  tabTitleDiv <- select ("#" <> (show tid) <> " > .tab-title")
  let
    newTitle = case cinfo.status of
      Just "loading" -> "Loading ..."
      _ -> tab.title
  setText newTitle tabTitleDiv
