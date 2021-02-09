module PureTabs.Sidebar.Bar where

import Browser.Tabs (Tab(..), TabId)
import Control.Alternative (pure)
import Control.Bind (bind, discard, map, void, (<#>))
import Data.Array ((:))
import Data.Array as A
import Data.Eq ((/=))
import Data.Function (($))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.MediaType.Common (textPlain)
import Data.Number (fromString)
import Data.Set (toUnfoldable, Set) as S
import Data.Set.NonEmpty (cons, max) as NES
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as T
import Data.Unit (Unit, unit)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (class Eq, class Ord, class Show, flip, show, (#), (&&), (+), (-), (<<<), (<>), (==), (>>>))
import PureTabs.Model.Events (SidebarEvent(..))
import PureTabs.Sidebar.Tabs (Output(..))
import PureTabs.Sidebar.Tabs as Tabs
import Sidebar.Component.GroupName as GroupName
import Sidebar.Utils (moveElem, whenC)
import Web.HTML.Event.DataTransfer as DT
import Web.HTML.Event.DragEvent as DE


newtype GroupId
  = GroupId Int

derive instance eqGroupId :: Eq GroupId
derive instance ordGroupId :: Ord GroupId

instance showGroupId :: Show GroupId where 
  show (GroupId gid) = "GroupId(" <> (show gid) <> ")"

type Group
  = { name :: String
    , pos :: Int
    }

type State
  = { groups :: M.Map GroupId Group
    , tabsToGroup :: M.Map TabId GroupId
    , groupTabsPositions :: Array (Tuple TabId GroupId)
    , currentGroup :: GroupId
    , draggedCurrentGroup :: Maybe GroupId
    }

data Action
  = UserSelectedGroup GroupId
  | UserRenameGroup GroupId String
  | UserCreatedGroup
  | UserDeletedGroup GroupId
  | HandleTabsOutput GroupId Tabs.Output
  | GroupNameDragOver DE.DragEvent GroupId
  | DragEnd DE.DragEvent

initialState :: forall i. i -> State
initialState _ =
  let
    firstGroupId = GroupId 0
  in
    { 
      groups: M.fromFoldable [ Tuple firstGroupId { name: "main", pos: 0 } ]
      , tabsToGroup: M.empty
      , groupTabsPositions : []
      , currentGroup: firstGroupId 
      , draggedCurrentGroup: Nothing
    }

type Slots
  = ( tab :: H.Slot Tabs.Query Tabs.Output GroupId, groupName :: forall unusedQuery. H.Slot unusedQuery GroupName.NewName GroupId)

_tab :: SProxy "tab"
_tab = (SProxy :: _ "tab")

_groupName :: SProxy "groupName"
_groupName = (SProxy :: _ "groupName")

component :: forall i m. MonadEffect m => MonadAff m => H.Component HH.HTML Tabs.Query i SidebarEvent m
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
  where

  render :: State -> H.ComponentHTML Action Slots m
  render state = 
    let 
        currentGroupShown = fromMaybe state.currentGroup state.draggedCurrentGroup

        menuElem attrs text = HH.li attrs [ HH.text text]

        topMenu = HH.div [ HP.id_ "bar-menu" ] [
          HH.ul [] [menuElem [HE.onClick \_ -> Just UserCreatedGroup] "+", menuElem [] "-"]
        ]

        barListGroup = HH.div [ HP.id_ "bar-list" ] [HH.ul [ HP.id_ "bar-list-group"] $ 
          (M.toUnfoldable state.groups) <#> \(Tuple gid g) -> renderGroup gid (gid == currentGroupShown) g
        ]

        tabsDivs = (S.toUnfoldable $ (M.keys state.groups)) <#> 
          (\gid -> HH.div [
            HP.classes [(H.ClassName "bar-tabs"), whenC (gid == currentGroupShown) (H.ClassName "bar-tabs-active")] 
          ] [renderGroupTabs gid])
    
     in
        HH.div [ HP.id_ "bar", HE.onDragEnd \evt -> Just $ DragEnd evt ] $ topMenu : barListGroup : tabsDivs 

  renderGroupTabs :: GroupId -> H.ComponentHTML Action Slots m
  renderGroupTabs groupId = HH.slot _tab groupId Tabs.component unit (Just <<< (HandleTabsOutput groupId))

  renderGroup :: GroupId -> Boolean -> Group -> H.ComponentHTML Action Slots m
  renderGroup groupId isActive group =  
    HH.li [ 
      HP.classes [(H.ClassName "group-name"), whenC isActive (H.ClassName "active-group")]
      , HE.onClick (\_ -> Just (UserSelectedGroup groupId))
      , HE.onDragOver \evt -> Just $ GroupNameDragOver evt groupId
    ] [ HH.slot _groupName groupId GroupName.component group.name (\newName -> Just (UserRenameGroup groupId newName))] 

  handleAction :: MonadEffect m => Action -> H.HalogenM State Action Slots SidebarEvent m Unit
  handleAction = 
    case _ of

         UserSelectedGroup gid -> do
            H.modify_ _ { currentGroup = gid }

         UserRenameGroup gid newName -> 
            H.modify_ \s -> s { groups = M.update (\g -> Just $ g { name = newName }) gid s.groups }

         UserCreatedGroup -> do
           H.modify_ \s -> 
             s { groups = 
               M.insert 
                 (findNextGroupId $ M.keys s.groups) 
                 { name: "new group", pos: M.size s.groups } 
                 s.groups 
               }

         UserDeletedGroup gid -> pure unit

         GroupNameDragOver dragEvent gid -> do
           let 
               dataTransfer = DE.dataTransfer dragEvent
           dragData <- H.liftEffect $ DT.getData textPlain dataTransfer
           case fromString dragData of
                Nothing -> H.liftEffect $ log $ "sb: group drag over, got something else than a number: " <> dragData
                Just tid -> do 
                   H.modify_ _ { draggedCurrentGroup = Just gid }
                   H.liftEffect $ log $ "sb: dragging " <> (show tid) <> " over " <> (show gid)

         DragEnd evt -> do 
            H.modify_ _ { draggedCurrentGroup = Nothing }
            H.liftEffect $ log $ "sb: drag end from bar component"

         HandleTabsOutput gid output -> 
           case output of 
              OutputTabDragEnd tid' -> do 
                   s <- H.get
                   case Tuple tid' s.draggedCurrentGroup of 
                        -- Only perform a move when we're dragging a tab onto a different group
                        Tuple (Just tid) (Just draggedGroup) | s.currentGroup /= draggedGroup -> 
                                 moveTabToGroup tid gid draggedGroup s
                        _ -> pure unit

                   H.modify_ _ { draggedCurrentGroup = Nothing }


              TabsSidebarAction (SbMoveTab tid groupIndex) -> sidebarMoveTab tid gid groupIndex
              TabsSidebarAction sbEvent -> H.raise sbEvent

    where
          findNextGroupId :: S.Set GroupId -> GroupId
          findNextGroupId values = 
            let GroupId(maxValue) = NES.max (NES.cons (GroupId 0) values)
             in GroupId(maxValue + 1)

          moveTabToGroup 
            :: MonadEffect m => TabId 
            -> GroupId 
            -> GroupId 
            -> State 
            -> H.HalogenM State Action Slots SidebarEvent m Unit
          moveTabToGroup tid fromGroup toGroup state = do
            let 
                -- XXX: The goal is to put it at the end, but if you:
                --  - create a new group
                --  - drag a tab from the first one to it
                --  - drag it back to the first group
                --  Then it will be at the beginning of the group, not the end.

                -- Right now we only put it at the end of the list. 
                -- We don't support dragging at a specific place.
                newTabIndex = 
                  fromMaybe (A.length state.groupTabsPositions) 
                  $ lastWinTabIndexInGroup toGroup state.groupTabsPositions

            s <- H.modify \s -> 
              s { tabsToGroup = M.update (\_ -> Just toGroup) tid s.tabsToGroup
              , groupTabsPositions = 
                s.groupTabsPositions
                <#> 
                (\(Tuple tid' gid') -> if tid' == tid then Tuple tid' toGroup else Tuple tid' gid') 
              -- Reassign the current group directly here to avoid flickering
              , currentGroup = toGroup
              }
            let newIndexInGroup = getPositionTabInGroup newTabIndex toGroup s.groupTabsPositions

            deletedTab' <- H.query _tab fromGroup $ H.request $ Tabs.TabDeleted tid
            case deletedTab' of 
                 Just (Just (Tab tab)) -> 
                   void $ H.query _tab toGroup $ H.tell 
                    $ Tabs.TabCreated $ Tab (tab { index = newIndexInGroup })
                 _ -> pure unit

            H.raise $ SbMoveTab tid newTabIndex
            H.raise $ SbActivateTab tid

          sidebarMoveTab 
            :: TabId 
            -> GroupId 
            -> Int 
            -> H.HalogenM State Action Slots SidebarEvent m Unit
          sidebarMoveTab tid gid groupIndex = do
             s <- H.get
             let 
                 oldPosition = getPositionTab tid gid s.groupTabsPositions
                 newIndex = do 
                    prevIdx <- oldPosition
                    s.groupTabsPositions #
                      A.mapWithIndex (Tuple) 
                            >>> A.filter (\(Tuple _ (Tuple _ gid')) -> gid' == gid)
                            >>> (flip A.index) groupIndex
                            >>> map T.fst

             -- Important: we ask Firefox to do the move, but we don't
             -- perform it ourselves.  This means we don't update the state.
             -- We will get back a TabMoved event that will then be
             -- processed accordingly.
             newIndex # maybe (pure unit) \idx -> H.raise $ SbMoveTab tid idx 

 

  handleQuery :: forall act a. Tabs.Query a -> H.HalogenM State act Slots SidebarEvent m (Maybe a)
  handleQuery = case _ of

    Tabs.InitialTabList tabs a -> do
       s <- H.modify (\s -> 
         let 
             tabIdGroup = tabs <#> \(Tab t) -> Tuple t.id s.currentGroup
          in
         s 
         { tabsToGroup = M.fromFoldable tabIdGroup
         , groupTabsPositions = tabIdGroup
         }
       )
       let activatedTab = tabs # A.filter (\(Tab t) -> t.active) >>> A.head
       void $ tellChild s.currentGroup $ Tabs.InitialTabList tabs
       activatedTab # maybe (pure unit) \(Tab t) -> void $ tellChild s.currentGroup $ Tabs.TabActivated Nothing t.id
       pure (Just a)

    Tabs.TabCreated (Tab tab) a -> do 
       s <- H.get

       let newGroupTabsPositions = 
             fromMaybe s.groupTabsPositions 
             $ A.insertAt tab.index (Tuple tab.id s.currentGroup) s.groupTabsPositions

           inGroupPosition = getPositionTabInGroup tab.index s.currentGroup newGroupTabsPositions 

           newTab = Tab $ tab { index = inGroupPosition }

       newS <- H.modify \state -> 
         state 
         { tabsToGroup = M.insert tab.id s.currentGroup s.tabsToGroup 
         , groupTabsPositions = newGroupTabsPositions
         }

       void $ tellChild newS.currentGroup $ Tabs.TabCreated newTab
       pure (Just a)

    Tabs.TabDeleted tid reply -> do 
       doOnTabGroup tid \gid -> do 
         H.modify_ (\s -> s 
                       { tabsToGroup = M.delete tid s.tabsToGroup 
                       , groupTabsPositions = A.deleteBy 
                          -- This is ugly. There is no function to delete the
                          -- first element of an array that matches a condition.
                          (\(Tuple tid1 _) (Tuple tid2 _) -> tid1 == tid2)
                          (Tuple tid s.currentGroup)
                          s.groupTabsPositions
                       })
         void $ H.query _tab gid $ H.request $ Tabs.TabDeleted tid
       pure (Just (reply Nothing))

    Tabs.TabActivated prevTid' tid a -> do 
       case prevTid' of
            mPrevTid @ (Just prevTid) -> doOnTabGroup prevTid \gid -> 
                void $ tellChild gid $ Tabs.TabActivated mPrevTid tid
            Nothing -> pure unit
       doOnTabGroup tid \gid -> do 
         { tabsToGroup } <- H.modify (_ { currentGroup = gid})
         H.raise $ SbSelectedGroup $ getTabIdsOfGroup gid tabsToGroup
         void $ tellChild gid $ Tabs.TabActivated prevTid' tid
       pure (Just a)

    Tabs.TabMoved tid next a -> do 
       doOnTabGroup tid \gid -> do 
         { groupTabsPositions } <- H.get 
         let 
             newGroupTabsPositions = fromMaybe groupTabsPositions $ do 
               prevPosition <- getPositionTab tid gid groupTabsPositions
               moveElem prevPosition next groupTabsPositions

             nextGroupPosition = getPositionTabInGroup next gid newGroupTabsPositions

         H.modify_ (_ { groupTabsPositions = newGroupTabsPositions })
         void $ tellChild gid $ Tabs.TabMoved tid nextGroupPosition
       pure (Just a)

    Tabs.TabInfoChanged tid cinfo a -> do 
       doOnTabGroup tid \gid -> do
         void $ tellChild gid $ Tabs.TabInfoChanged tid cinfo
       pure (Just a)

    Tabs.TabDetached tid a -> do 
       handleQuery $ Tabs.TabDeleted tid \_ -> a

    Tabs.TabAttached tab a -> do 
       handleQuery $ Tabs.TabCreated tab a

    where
        tellChild :: GroupId -> (H.Tell Tabs.Query) -> H.HalogenM State act Slots SidebarEvent m (Maybe Unit)
        tellChild gid q = H.query _tab gid $ H.tell q
        -- 
        -- requestChild :: GroupId -> (H.Request Tabs.Query) -> H.HalogenM State act Slots SidebarEvent M (Maybe a)
        -- requestChild gid q = H.request 

        doOnTabGroup 
          :: TabId 
          -> (GroupId -> H.HalogenM State act Slots SidebarEvent m Unit) 
          -> H.HalogenM State act Slots SidebarEvent m Unit
        doOnTabGroup tabId f = do
          { tabsToGroup } <- H.get
          case M.lookup tabId tabsToGroup of 
               Just groupId -> f groupId
               Nothing -> pure unit

getPositionTabInGroup
  :: Int
  -> GroupId
  -> Array (Tuple TabId GroupId)
  -> Int
getPositionTabInGroup index gid = 
  (A.take $ index + 1)
     >>> (A.filter \(Tuple _ gid') -> gid' == gid)  
     >>> A.length
     >>> (flip (-) $ 1)

getPositionTab 
  :: TabId
  -> GroupId
  -> Array (Tuple TabId GroupId)
  -> Maybe Int
getPositionTab tid gid arr = A.findIndex (\(Tuple tid' gid') -> tid' == tid && gid' == gid) arr

getTabIdsOfGroup 
  :: GroupId
  -> M.Map TabId GroupId
  -> Array TabId
getTabIdsOfGroup gid =
  M.toUnfoldable 
  >>> A.filter (\(Tuple tid gid') -> gid' == gid)
  >>> map T.fst

--| Obtain the window index of the last tab of a group.
lastWinTabIndexInGroup 
  :: GroupId
  -> Array (Tuple TabId GroupId)
  -> Maybe Int
lastWinTabIndexInGroup gid = 
  A.mapWithIndex (Tuple)
    >>> A.filter (T.snd >>> T.snd >>> (==) gid)
    >>> map T.fst
    >>> A.head

