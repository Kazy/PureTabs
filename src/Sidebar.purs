module PureTabs.Sidebar where

import Browser.Runtime as Runtime
import Browser.Tabs (Tab(..), TabId, WindowId)
import Browser.Tabs.OnUpdated (ChangeInfo(..))
import Browser.Windows (getCurrent)
import Control.Alternative (pure)
import Control.Bind ((>=>), (>>=))
import Data.Foldable (traverse_)
import Data.Function (flip)
import Data.Lens (view)
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
import JQuery as J
import Prelude (Unit, bind, ($), discard)
import PureTabs.Model (BackgroundEvent(..), SidebarEvent(..), _tabId)

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
  tabsDiv <- J.select "#tabs"
  _ <- Runtime.onMessageJsonAddListener port $ onMsg tabsDiv
  pure unit
  where
  onMsg :: J.JQuery -> BackgroundEvent -> Effect Unit
  onMsg contentDiv event = case event of
    BgTabCreated tab -> do
      tabElem <- createTabElement port tab
      J.append tabElem contentDiv
      pure unit
    BgTabDeleted tabId -> deleteTabElement tabId
    BgInitialTabList tabs -> traverse_ (createTabElement port >=> (flip J.append) contentDiv) tabs
    BgTabUpdated tid cinfo tab -> updateTabInfo tid cinfo tab
    BgTabActived prev new -> activateTab prev new
    _ -> log "received unsupported message type"

createTabElement :: Runtime.Port -> Tab -> Effect J.JQuery
createTabElement port tab' = do
  let
    tab = unwrap tab'
  tabDiv <- J.create "<div>"
  J.setAttr "class" "tab" tabDiv
  J.setAttr "id" tab.id tabDiv
  J.on "click" onTabClick tabDiv
  if tab.active then (J.addClass "active" tabDiv) else (pure unit)
  -- favicon
  faviconDiv <- J.create "<div>"
  J.addClass "tab-favicon" faviconDiv
  setFaviconUrl tab.favIconUrl faviconDiv
  J.append faviconDiv tabDiv
  -- title
  tabTitle <- J.create "<div>"
  J.addClass "tab-title" tabTitle
  J.setText tab.title tabTitle
  J.append tabTitle tabDiv
  -- close button
  closeButton <- createCloseButton
  J.append closeButton tabDiv
  J.on "click" onCloseClick closeButton
  pure tabDiv
  where
  onCloseClick :: J.JQueryEvent -> J.JQuery -> Effect Unit
  onCloseClick event j = Runtime.postMessageJson port $ SbTabDeleted $ view _tabId tab'

  onTabClick :: J.JQueryEvent -> J.JQuery -> Effect Unit
  onTabClick event j = Runtime.postMessageJson port $ SbTabActived $ view _tabId tab'

createCloseButton :: Effect J.JQuery
createCloseButton = do
  parent <- J.create "<div>"
  J.addClass "close-button-parent" parent
  outer <- J.create "<div>"
  J.addClass "close-button-outer" outer
  J.append outer parent
  inner <- J.create "<div>"
  J.addClass "close-button-inner" inner
  J.append inner outer
  pure parent

setFaviconUrl :: Maybe String -> J.JQuery -> Effect Unit
setFaviconUrl Nothing div = pure unit

setFaviconUrl (Just favData) div = J.css { "background-image": favUrl } div
  where
  favUrl = "url(" <> favData <> ")"

deleteTabElement :: TabId -> Effect Unit
deleteTabElement tabId = do
  div <- J.select ("#" <> show tabId)
  J.remove div

updateTabInfo :: TabId -> ChangeInfo -> Tab -> Effect Unit
updateTabInfo tid (ChangeInfo cinfo) (Tab tab) = do
  tabTitleDiv <- J.select ("#" <> (show tid) <> " > .tab-title")
  let
    newTitle = case cinfo.status of
      Just "loading" -> Just "Loading ..."
      _ -> Just tab.title
  maybe (pure unit) (\t -> J.setText t tabTitleDiv) newTitle
  tabFaviconDiv <- J.select ("#" <> (show tid) <> " > .tab-favicon")
  setFaviconUrl cinfo.favIconUrl tabFaviconDiv

activateTab :: (Maybe TabId) -> TabId -> Effect Unit
activateTab prev new = do
  maybe (pure unit) (\p -> (J.select ("#" <> (show p))) >>= J.setClass "active" false) prev
  newTab <- J.select ("#" <> (show new))
  J.setClass "active" true newTab
