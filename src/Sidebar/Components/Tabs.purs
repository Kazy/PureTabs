module PureTabs.Sidebar.Tabs (component, Query(..)) where

import Browser.Tabs (Tab(..), TabId)
import Browser.Tabs.OnUpdated (ChangeInfo(..), ChangeInfoRec)
import CSS.Background as CssBackground
import Control.Alt ((<$>))
import Control.Alternative (empty, pure, (*>))
import Control.Bind (bind, discard, (>=>), (>>=))
import Control.Category (identity, (<<<), (>>>))
import Data.Array (mapWithIndex, catMaybes, deleteAt, filter, findIndex, head, insertAt, modifyAt, (!!)) as A
import Data.Eq ((/=), (==))
import Data.Function (flip, ($))
import Data.Lens (over)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.MediaType.Common (textPlain)
import Data.Monoid ((<>))
import Data.Show (show)
import Data.Symbol (SProxy(..))
import Data.Unit (Unit, unit)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PureTabs.Model (SidebarEvent(..), _tabs)
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.HTML.Event.DataTransfer as DT
import Web.HTML.Event.DragEvent as DE
import Web.UIEvent.MouseEvent as ME

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
  | UserOpenedTab Event
  -- drags
  | TabDragStart DE.DragEvent Tab Int
  | TabDragOver DE.DragEvent Int
  | TabDragEnd DE.DragEvent
  -- mouse event
  | TabMouseEnter ME.MouseEvent Int
  | TabMouseLeave ME.MouseEvent Int

type DraggedTab
  = { tab :: Tab
    , originalIndex :: Int
    , overIndex :: Int
    }

type State
  = { tabs :: Array Tab
    , selectedElem :: Maybe DraggedTab
    , tabHovered :: Maybe Int
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
initialState _ = { tabs: empty, selectedElem: Nothing, tabHovered: Nothing }

_tab :: SProxy "tab"
_tab = SProxy

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    tabsWithIndex = state.tabs

    tabs =
      fromMaybe tabsWithIndex
        $ state.selectedElem
        >>= ( \{ originalIndex, overIndex } -> moveElem originalIndex overIndex tabsWithIndex
          )
  in
    HH.div
      [ HP.id_ "tabs"
      , HE.onDoubleClick (\ev -> Just (UserOpenedTab $ ME.toEvent ev))
      ]
      (A.mapWithIndex renderTab tabs)
  where
  renderTab index (Tab t) =
    HH.div
      [ HP.id_ $ show t.id
      , HP.draggable true
      -- drag events
      , HE.onDragStart \evt -> Just $ TabDragStart evt (Tab t) index
      , HE.onDragEnd \evt -> Just $ TabDragEnd evt
      , HE.onDragOver \evt -> Just $ TabDragOver evt index
      -- fake hover
      , HE.onMouseEnter \evt -> Just $ TabMouseEnter evt index
      , HE.onMouseLeave \evt -> Just $ TabMouseLeave evt index
      -- click event
      , HE.onClick (\ev -> Just (UserActivatedTab t.id (ME.toEvent ev)))
      -- TODO: on double click on a tab, open a tab right below
      -- clases
      , HP.classes $ H.ClassName
          <$> A.catMaybes
              [ Just "tab"
              , if t.active then Just "active" else Nothing
              , if isDiscarded t then Just "discarded" else Nothing
              , case state.tabHovered of
                  Just idx
                    | idx == index -> Just "hover"
                  _ -> Nothing
              ]
      , HP.title t.title
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
    H.liftEffect
      $ do
          Event.preventDefault ev
          Event.stopPropagation ev
          log "sb: closed a tab"
    H.raise $ SbDeleteTab tid
  UserActivatedTab tid ev -> do
    H.liftEffect
      $ do
          Event.preventDefault ev
          Event.stopPropagation ev
          log "sb: activated a tab"
    H.raise $ SbActivateTab tid
  UserOpenedTab ev -> do
    H.liftEffect
      $ do
          Event.preventDefault ev
          Event.stopPropagation ev
          log "sb: created a tab"
    H.raise SbCreateTab
  -- Drag actions
  TabDragStart dragEvent tab index -> do
    let
      dataTransfer = DE.dataTransfer dragEvent
    H.liftEffect
      $ do
          DT.setData textPlain "" dataTransfer
          DT.setDropEffect DT.Move dataTransfer
    H.modify_ _ { selectedElem = Just { tab: tab, originalIndex: index, overIndex: index }, tabHovered = Nothing }
    H.liftEffect $ log $ "sb: drag start from " <> (show index)
  TabDragOver event index -> do
    -- prevent the ghost from flying back to its (wrong) place
    -- see https://stackoverflow.com/questions/42725321/prevent-html5-drag-ghost-image-flying-back
    H.liftEffect $ Event.preventDefault (DE.toEvent event)
    state <- H.get
    case state.selectedElem of
      Just selectedRec@{ originalIndex, overIndex }
        | overIndex /= index -> do
          H.modify_ (_ { selectedElem = Just $ selectedRec { overIndex = index } })
      _ -> pure unit
  TabDragEnd event -> do
    state <- H.get
    case state.selectedElem of
      Nothing -> pure unit
      Just { tab: (Tab t), originalIndex, overIndex } -> H.raise (SbMoveTab t.id overIndex)
  -- Mouse over action
  TabMouseEnter evt index -> do
    state <- H.get
    case state of
      { tabHovered: Nothing, selectedElem: Nothing } -> H.modify_ _ { tabHovered = Just index }
      _ -> pure unit
  TabMouseLeave evt index -> do
    state <- H.get
    case state.tabHovered of
      Nothing -> pure unit
      Just prevIdx -> H.modify_ _ { tabHovered = Nothing }

handleQuery :: forall act o m a. Query a -> H.HalogenM State act () o m (Maybe a)
handleQuery = case _ of
  InitialTabList tabs a -> H.modify_ (\s -> s { tabs = tabs }) *> pure (Just a)
  TabCreated (Tab t) a ->
    H.modify_
      (over _tabs $ \tabs -> fromMaybe tabs $ A.insertAt t.index (Tab t) tabs)
      *> pure (Just a)
  TabDeleted tid a ->
    H.modify_
      ( over _tabs
          $ applyAtTabId tid A.deleteAt
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
    maybeFlipped tab' (pure unit) \tab -> do
      H.modify_
        ( over _tabs \tabs ->
            fromMaybe tabs $ (A.deleteAt prev >=> A.insertAt next tab) tabs
        )
      -- Wait for a move to disable the drag data, otherwise the tab will come
      -- back briefly to its original place before switching again.
      -- This also means that if the move fail, this will be in an inconsistant
      -- state.
      H.modify_ \s -> s { selectedElem = Nothing }
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

moveElem :: forall a. Int -> Int -> Array a -> Maybe (Array a)
moveElem from to arr = do
  elem <- arr A.!! from
  (A.deleteAt from >=> A.insertAt to elem) arr
