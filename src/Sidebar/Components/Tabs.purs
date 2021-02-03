module PureTabs.Sidebar.Tabs (component, Query(..), Output(..)) where

import Browser.Tabs (Tab(..), TabId, showTabId)
import Browser.Tabs.OnUpdated (ChangeInfo(..), ChangeInfoRec)
import CSS.Background as CssBackground
import Control.Alt ((<$>))
import Control.Alternative (empty, pure, (*>))
import Control.Bind (bind, discard, (>=>), (>>=))
import Control.Category (identity, (<<<), (>>>))
import Data.Array (mapWithIndex, catMaybes, deleteAt, filter, findIndex, head, insertAt, modifyAt, (!!), length) as A
import Data.Eq ((/=), (==))
import Data.Function (flip, ($))
import Data.Lens (over)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.MediaType.Common (textPlain)
import Data.Monoid ((<>))
import Data.Show (show)
import Data.String.CodeUnits (length)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Unit (Unit, unit)
import Effect.AVar (AVar)
import Effect.Aff (Aff, Fiber, forkAff, delay, killFiber)
import Effect.Aff.AVar (put, empty, take) as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (negate, sub)
import PureTabs.Model.Events (SidebarEvent(..))
import PureTabs.Model.GlobalState (_tabs)
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
  | TabDetached TabId a
  | TabAttached Tab a

data Output 
  = TabsSidebarAction SidebarEvent

data Action
  = UserClosedTab TabId Event
  | UserActivatedTab TabId Event
  | UserOpenedTab (Maybe TabId) Event
  -- drags
  | TabDragStart DE.DragEvent Tab Int
  | TabDragOver DE.DragEvent Int
  | TabDragEnd DE.DragEvent
  | TabDragLeave DE.DragEvent
  | TabDragLeaveRun DE.DragEvent
  -- mouse event
  | TabMouseEnter ME.MouseEvent Int
  | TabMouseLeave ME.MouseEvent Int
  -- special
  -- stop the propagation of the event
  | PreventPropagation Event

type DraggedTab
  = { tab :: Tab
    , originalIndex :: Int
    , overIndex :: Maybe Int
    }

-- Debouncer based on https://gist.github.com/natefaubion/3405f930b9008e52e5d995681a7d6f2b
type Debouncer
  = { var :: AVar Unit
    , timer :: Fiber Unit
    }


type State
  = { tabs :: Array Tab
    , selectedElem :: Maybe DraggedTab
    , tabHovered :: Maybe Int
    , leaveDebounce :: Maybe Debouncer
    }

component :: forall i m. MonadEffect m => MonadAff m => H.Component HH.HTML Query i Output m
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
initialState _ = { tabs: empty, selectedElem: Nothing, tabHovered: Nothing, leaveDebounce: Nothing }

debounceTimeout :: Milliseconds -> AVar Unit -> Aff (Fiber Unit)
debounceTimeout ms var =
  forkAff do
    delay ms
    AVar.put unit var

_tab :: SProxy "tab"
_tab = SProxy

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    tabsWithIndex = state.tabs

    tabs =
      fromMaybe tabsWithIndex
        $ state.selectedElem
        >>= ( \{ originalIndex, overIndex } -> case overIndex of
              Just overIndex' -> moveElem originalIndex overIndex' tabsWithIndex
              Nothing -> A.deleteAt originalIndex tabsWithIndex
          )

    currentOverIndex = fromMaybe (-1) $ state.selectedElem >>= _.overIndex
  in
    HH.div
      [ HP.id_ "tabs"
      , HE.onDoubleClick (\ev -> Just (UserOpenedTab Nothing (ME.toEvent ev)))
      , HE.onDragOver \evt -> Just $ TabDragOver evt (sub (A.length tabs) 1)
      , HE.onDragLeave \evt -> Just $ TabDragLeave evt
      ]
      [ HH.div
          [ HP.id_ "inner-tabs"
          -- We prevent both propagation to avoid tabs blinking during drag and
          -- drop. In the case of dragOver, the handler from #tabs triggers
          -- when we drag over between two tabs (because of the margin), and
          -- the tab jumps brefiely to the end. 
          -- The same happens for dragLeave, but with the tab disappearing
          -- brefiely.
          , HE.onDragOver \evt -> Just $ PreventPropagation $ DE.toEvent evt
          , HE.onDragLeave \evt -> Just $ TabDragLeave evt
          ]
          (A.mapWithIndex (\idx tab -> renderTab idx (idx == currentOverIndex) tab) tabs)
      ]

  where

  threeDotBounces = HH.div [ HP.class_ (H.ClassName "three-dot-bounce") ] [
    HH.div [HP.class_ (H.ClassName "three-dot-bounce-1")] [],  
    HH.div [HP.class_ (H.ClassName "three-dot-bounce-2")] [],
    HH.div [HP.class_ (H.ClassName "three-dot-bounce-3")] []
    ]

  renderTab :: Int -> Boolean -> Tab -> H.ComponentHTML Action () m
  renderTab index isBeingDragged (Tab t) =
    HH.div
      [ HP.id_ $ show t.id
      , HP.draggable true

      -- drag events
      , HE.onDragStart \evt -> Just $ TabDragStart evt (Tab t) index
      , HE.onDragEnd \evt -> Just $ TabDragEnd evt
      , HE.onDragOver \evt -> Just $ TabDragOver evt index

      -- fake hover to fix incorrect css hover effect during dragging
      , HE.onMouseEnter \evt -> Just $ TabMouseEnter evt index
      , HE.onMouseLeave \evt -> Just $ TabMouseLeave evt index

      -- click event
      , HE.onClick (\ev -> Just (UserActivatedTab t.id (ME.toEvent ev)))
      , HE.onDoubleClick (\ev -> Just (UserOpenedTab (Just t.id) (ME.toEvent ev)))

      -- classes
      , HP.classes $ H.ClassName
          <$> A.catMaybes
              [ Just "tab"
              , if t.active then Just "active" else Nothing
              , if isDiscarded t then Just "discarded" else Nothing
              , if isBeingDragged then Just "being-dragged" else Nothing
              , case state.tabHovered of
                  Just idx
                    | idx == index -> Just "hover"
                  _ -> Nothing
              ]
      , HP.title t.title
      ] [
      case t.status of 
           Just "loading" -> threeDotBounces
           _ -> HH.div [ HP.class_ $ H.ClassName "tab-favicon", faviconStyle t.favIconUrl ] [] 

      , HH.div [ HP.class_ $ H.ClassName "tab-title" ] [ HH.text (if length t.title /= 0 then t.title else maybe "" identity t.url) ]

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

cancelLeaveDebounce :: forall m. MonadAff m => State -> H.HalogenM State Action () Output m Unit
cancelLeaveDebounce state = case state.leaveDebounce of
  Just { var, timer } -> do
    H.liftAff $ killFiber (error "could not cancel timer") timer
    H.modify_ _ { leaveDebounce = Nothing }
  Nothing -> pure unit

runDebounce :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
runDebounce actionToRun = do
  state <- H.get
  let
    debounceTime = Milliseconds 50.0
  case state.leaveDebounce of
    Nothing -> do
      var <- H.liftAff AVar.empty
      timer <- H.liftAff (debounceTimeout debounceTime var)
      _ <-
        H.fork do
          H.liftAff (AVar.take var)
          H.modify_ _ { leaveDebounce = Nothing }
          handleAction actionToRun
      let
        debouncer = { var, timer }
      H.modify_ _ { leaveDebounce = Just debouncer }

    Just { var, timer } -> do
      H.liftAff $ killFiber (error "could not cancel timer") timer
      nextTimer <- H.liftAff (debounceTimeout debounceTime var)
      let
        debouncer = { var, timer: nextTimer }
      H.modify_ _ { leaveDebounce = Just debouncer }

handleAction :: forall m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of

  UserClosedTab tid ev -> do
    H.liftEffect
      $ do
          Event.preventDefault ev
          Event.stopPropagation ev
          log "sb: closed a tab"
    H.raise $ TabsSidebarAction $ SbDeleteTab tid

  UserActivatedTab tid ev -> do
    H.liftEffect
      $ do
          Event.preventDefault ev
          Event.stopPropagation ev
          log "sb: activated a tab"
    H.raise $ TabsSidebarAction $ SbActivateTab tid

  UserOpenedTab tid ev -> do
    H.liftEffect
      $ do
          Event.preventDefault ev
          Event.stopPropagation ev
          log "sb: created a tab"
    H.raise $ TabsSidebarAction $ SbCreateTab tid

  -- Drag actions
  TabDragStart dragEvent tab index -> do
    let
      dataTransfer = DE.dataTransfer dragEvent
    H.liftEffect
      $ do
          DT.setData textPlain "" dataTransfer
          DT.setDropEffect DT.Move dataTransfer
    H.modify_ _ { selectedElem = Just { tab: tab, originalIndex: index, overIndex: Just index }, tabHovered = Nothing }
    H.liftEffect $ log $ "sb: drag start from " <> (show index)

  TabDragOver event index -> do
    -- prevent the ghost from flying back to its (wrong) place
    -- see https://stackoverflow.com/questions/42725321/prevent-html5-drag-ghost-image-flying-back
    let
      evt = (DE.toEvent event)
    H.liftEffect $ Event.preventDefault evt
    -- because we're also triggering this event on over of the empty part of the
    -- tab list, we need to prevent it from triggering twice.
    H.liftEffect $ Event.stopPropagation evt
    state <- H.get
    cancelLeaveDebounce state
    case state.selectedElem of
      Just selectedRec@{ originalIndex, overIndex } -> case overIndex of
        -- we only do nothing if we're still over the same element
        Just overIndex'
          | overIndex' == index -> pure unit
        _ -> H.modify_ (_ { selectedElem = Just $ selectedRec { overIndex = Just index } })
      Nothing -> pure unit

  PreventPropagation event -> do
    H.liftEffect $ Event.stopImmediatePropagation event
    pure unit

  TabDragEnd event -> do
    state <- H.get
    cancelLeaveDebounce state
    case state.selectedElem of
      Nothing -> pure unit
      -- On success, we don't remove the dragged element here. It is instead done in the
      -- query handler for TabMoved. See comment there for the explanation.
      Just { tab: (Tab t), originalIndex, overIndex: (Just overIndex) } -> H.raise $ TabsSidebarAction (SbMoveTab t.id overIndex)
      Just { overIndex: Nothing } -> H.modify_ _ { selectedElem = Nothing }

  TabDragLeave event -> runDebounce $ TabDragLeaveRun event

  TabDragLeaveRun event -> do
    state <- H.get
    H.liftEffect $ log "actually running drag leave"
    case state.selectedElem of
      Just selectedRec@{ overIndex: (Just overIndex) } -> H.modify_ _ { selectedElem = Just $ selectedRec { overIndex = Nothing } }
      _ -> pure unit

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

handleQuery :: forall act o m a. MonadEffect m => Query a -> H.HalogenM State act () o m (Maybe a)
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

  TabDetached tid a -> 
    handleQuery $ TabDeleted tid a

  TabAttached tab a -> do
    H.liftEffect (log $ "sb: tab attached " <> (showTabId tab))
    handleQuery $ TabCreated tab a

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
