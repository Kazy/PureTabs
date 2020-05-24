module PureTabs.Sidebar.Tabs (component, Query(..)) where

import Browser.Tabs (Tab(..), TabId(..))
import Browser.Tabs.OnUpdated (ChangeInfo(..), ChangeInfoRec)
import CSS.Background as CssBackground
import Control.Alt ((<#>), (<$>), (<|>))
import Control.Alternative (empty, pure, (*>), (<*>))
import Control.Bind (bind, discard, (>=>), (>>=))
import Control.Category (identity, (<<<), (>>>))
import Data.Array (catMaybes, deleteAt, filter, findIndex, head, insertAt, modifyAt, (!!)) as A
import Data.Array (foldl)
import Data.Const (Const(..))
import Data.Eq ((==))
import Data.Function (const, flip, (#), ($))
import Data.Lens (over)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Show (show)
import Data.Symbol (SProxy(..))
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PureTabs.Model (BackgroundEvent(..), SidebarEvent(..))
import PureTabs.Model (_tabs)
import PureTabs.Sidebar.Tab as TabC
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (toEvent) as ME

data Query a
  = InitialTabList (Array Tab) a
  | TabCreated Tab a
  | TabDeleted TabId a
  | TabActivated (Maybe TabId) TabId a
  | TabMoved TabId Int Int a
  | TabInfoChanged TabId ChangeInfo a

data Action
  = UserClosedTab TabId Event
  | UserActivatedTab TabId Event

type State
  = { tabs :: Array Tab
    }

component :: forall i m. MonadEffect m => H.Component HH.HTML Query i SidebarEvent m
component =
  H.mkComponent
    { initialState
    , render: render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleQuery = handleQuery
              , handleAction = handleAction
              }
    }

initialState :: forall i. i -> State
initialState _ = { tabs: empty }

_tab :: SProxy "tab"
_tab = SProxy

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.id_ "tabs"
    ]
    (renderTab <$> state.tabs)
  where
  renderTab (Tab t) =
    HH.div
      [ HP.id_ $ show t.id
      , HE.onClick (\ev -> Just (UserActivatedTab t.id (ME.toEvent ev)))
      , HP.classes $ H.ClassName
          <$> A.catMaybes
              [ Just "tab"
              , if t.active then Just "active" else Nothing
              , if isDiscarded t then Just "discarded" else Nothing
              ]
      ]
      [ HH.div [ HP.class_ $ H.ClassName "tab-favicon", faviconStyle t.favIconUrl ] []
      , HH.div [ HP.class_ $ H.ClassName "tab-title" ]
          [ HH.text
              $ case t.status of
                  Just "loading" -> "Loading ..."
                  _ -> t.title
          ]
      , HH.div
          [ HP.class_ $ H.ClassName "close-button-parent"
          , HE.onClick (\ev -> Just (UserClosedTab t.id (ME.toEvent ev)))
          ]
          [ HH.div [ HP.class_ $ H.ClassName "close-button-outer" ]
              [ HH.div [ HP.class_ $ H.ClassName "close-button-inner" ] []
              ]
          ]
      ]

  faviconStyle favicon' =
    CSS.style
      $ do
          case favicon' of
            Nothing -> pure unit
            Just favicon -> CssBackground.backgroundImage $ CssBackground.url favicon

  isDiscarded :: forall r. { discarded :: Maybe Boolean | r } -> Boolean
  isDiscarded { discarded: Just true } = true

  isDiscarded _ = false

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () SidebarEvent m Unit
handleAction = case _ of
  UserClosedTab tid ev -> do
    H.liftEffect $ do 
        Event.preventDefault ev 
        Event.stopPropagation ev
    H.liftEffect $ log "sb: closed a tab"
    H.raise $ SbDeleteTab tid
  UserActivatedTab tid ev -> do
    H.liftEffect $ do 
        Event.preventDefault ev 
        Event.stopPropagation ev
    H.liftEffect $ log "sb: activated a tab"
    H.raise $ SbActivateTab tid

handleQuery :: forall act o m a. Query a -> H.HalogenM State act () o m (Maybe a)
handleQuery = case _ of
  InitialTabList tabs a -> H.put { tabs } *> pure (Just a)
  TabCreated (Tab t) a ->
    H.modify_
      (over _tabs $ \tabs -> fromMaybe tabs $ A.insertAt t.index (Tab t) tabs)
      *> pure (Just a)
  TabDeleted tid a ->
    H.modify_
      ( over _tabs
          $ applyAtTabId tid A.deleteAt
      {-- $ \tabs -> fromMaybe tabs $ findIndexTabId tid tabs >>= (flip A.deleteAt) tabs --}
      )
      *> pure (Just a)
  TabActivated oldTid tid a ->
    H.modify_
      ( over _tabs
          $ maybe identity (\old -> applyAtTabId old $ setTabActiveAtIndex false) oldTid
          >>> applyAtTabId tid (setTabActiveAtIndex true)
      )
      *> pure (Just a)
  TabMoved tid prev next a -> do
    state <- H.get
    let
      tab' = state.tabs A.!! prev
    maybeFlipped tab' (pure unit) \tab ->
      H.modify_
        ( over _tabs \tabs ->
            fromMaybe tabs $ (A.deleteAt prev >=> A.insertAt next tab) tabs
        )
    pure (Just a)
  TabInfoChanged tid cinfo a ->
    H.modify_
      ( over _tabs
          $ \tabs ->
              fromMaybe tabs
                $ (findIndexTabId tid >=> \index -> A.modifyAt index (updateTabFromInfo cinfo) tabs) tabs
      )
      *> pure (Just a)

setTabActive :: Boolean -> Tab -> Tab
setTabActive act (Tab t) = Tab (t { active = act })

setTabActiveAtIndex :: Boolean -> Int -> Array Tab -> Maybe (Array Tab)
setTabActiveAtIndex act i = A.modifyAt i (setTabActive act)

findTabByTabId :: TabId -> Array Tab -> Maybe Tab
findTabByTabId tid = A.head <<< A.filter \(Tab t) -> t.id == tid

findIndexTabId :: TabId -> Array Tab -> Maybe Int
findIndexTabId tid = A.findIndex \(Tab t) -> t.id == tid

applyAtTabId :: TabId -> (Int -> Array Tab -> Maybe (Array Tab)) -> Array Tab -> Array Tab
applyAtTabId tid f a = fromMaybe a $ findIndexTabId tid a >>= (flip f) a

maybeFlipped :: forall a b. Maybe a -> b -> (a -> b) -> b
maybeFlipped ma b f = maybe b f ma

updateTabFromInfo :: ChangeInfo -> Tab -> Tab
updateTabFromInfo (ChangeInfo cinfo) (Tab t) =
  let
    updateField :: forall r a. { acc :: ChangeInfoRec -> Maybe a, update :: a -> r -> r } -> r -> r
    updateField { acc, update } tab = case acc cinfo of
      Nothing -> tab
      Just field -> update field tab

    applyChange =
      updateField { acc: _.title, update: (\val -> _ { title = val }) }
        >>> updateField { acc: _.status, update: (\val -> _ { status = Just val }) }
        >>> updateField { acc: _.discarded, update: (\val -> _ { discarded = Just val }) }
        >>> updateField { acc: _.url, update: (\val -> _ { url = Just val }) }
        >>> updateField { acc: _.pinned, update: (\val -> _ { pinned = val }) }
        >>> updateField { acc: _.hidden, update: (\val -> _ { hidden = val }) }
        >>> updateField { acc: _.favIconUrl, update: (\val -> _ { favIconUrl = Just val }) }
  in
    Tab (applyChange t)
