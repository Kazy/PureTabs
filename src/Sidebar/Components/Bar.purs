module PureTabs.Sidebar.Bar where

import Browser.Tabs (Tab(..), TabId)
import Browser.Utils (eqBy, sortByKeyIndex)
import Control.Bind (bind, discard, map, void, (<#>), (>>=))
import Data.Array ((:))
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Eq ((/=))
import Data.Foldable (for_)
import Data.Function (($))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', maybe)
import Data.MediaType.Common (textPlain)
import Data.Number (fromString)
import Data.Set (Set, toUnfoldable) as S
import Data.Set.NonEmpty (cons, max) as NES
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, traverse)
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
import Prelude (flip, pure, show, (#), (&&), (+), (-), (<$>), (<<<), (<>), (==), (>), (>>>))
import PureTabs.Model.Group (GroupId(..))
import PureTabs.Model.GroupMapping (GroupData(..))
import PureTabs.Model.SidebarEvent (SidebarEvent(..))
import PureTabs.Model.TabWithGroup (TabWithGroup(..))
import PureTabs.Sidebar.Component.GroupName as GroupName
import PureTabs.Sidebar.Component.TopMenu as TopMenu
import PureTabs.Sidebar.Tabs (Output(..))
import PureTabs.Sidebar.Tabs as Tabs
import Sidebar.Utils (moveElem, whenC)
import Web.HTML.Event.DataTransfer as DT
import Web.HTML.Event.DragEvent as DE


-- TODO: correctly use `pos` when adding or deleting a group (i.e. making sure
-- the pos are contiguous from 0 to #groups - 1)
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
  | UserChangedDeletion Boolean
  | UserDeletedGroup GroupId
  | HandleTabsOutput GroupId Tabs.Output
  | GroupNameDragOver DE.DragEvent GroupId
  | DragEnd DE.DragEvent


data Query a
  = TabsQuery (Tabs.Query a)
  | InitialTabsWithGroup (Array GroupData) (Array TabWithGroup) a
  | InitializeGroups (Array GroupData) a
  | TabCreated Tab (Maybe GroupId) a
  | GroupDeleted GroupId (Maybe TabId) a

initialGroup :: M.Map GroupId Group
initialGroup = M.fromFoldable [ Tuple (GroupId 0) { name: "main", pos: 0 } ]

initialState :: forall i. i -> State
initialState _ =
  { groups: initialGroup
    , tabsToGroup: M.empty
    , groupTabsPositions : []
    , currentGroup: GroupId 0
    , draggedCurrentGroup: Nothing
    }

type Slots
  = ( tabs :: Tabs.Slot GroupId
    , groupName :: GroupName.Slot GroupId
    , topMenu :: TopMenu.Slot Unit)

_tabs :: SProxy "tabs"
_tabs = (SProxy :: _ "tabs")

_groupName :: SProxy "groupName"
_groupName = (SProxy :: _ "groupName")

_topMenu :: SProxy "topMenu"
_topMenu = (SProxy :: _ "topMenu")

component :: forall i m. MonadEffect m => MonadAff m => H.Component HH.HTML Query i SidebarEvent m
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

        topMenu = HH.slot _topMenu unit TopMenu.component unit (
          Just <<< case _ of 
               TopMenu.CreateGroup -> UserCreatedGroup
               TopMenu.ChangedDeletion value -> UserChangedDeletion value
        )

        -- TODO: order groups by `pos`
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
  renderGroupTabs groupId = HH.slot _tabs groupId Tabs.component unit (Just <<< (HandleTabsOutput groupId))

  renderGroup :: GroupId -> Boolean -> Group -> H.ComponentHTML Action Slots m
  renderGroup groupId isActive group =  
    HH.li [ 
      HP.classes [(H.ClassName "group-name"), whenC isActive (H.ClassName "active-group")]
      , HE.onClick (\_ -> Just (UserSelectedGroup groupId))
      , HE.onDragOver \evt -> Just $ GroupNameDragOver evt groupId
    ] [ HH.slot _groupName groupId GroupName.component group.name 
          case _ of 
               GroupName.NewName newName -> Just (UserRenameGroup groupId newName)
               GroupName.DeleteGroup -> Just (UserDeletedGroup groupId)
    ] 

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action Slots SidebarEvent m Unit
handleAction = 
  case _ of

       UserSelectedGroup gid -> do
          H.modify_ _ { currentGroup = gid }

       UserRenameGroup gid newName -> do
          H.modify_ \s -> s { groups = M.update (\g -> Just $ g { name = newName }) gid s.groups }
          H.raise $ SbRenamedGroup gid newName

       UserCreatedGroup -> do
          s <- H.get
          let Tuple gid newGroup = createGroup Nothing s
          H.modify_ $ insertGroup gid newGroup
          H.raise $ SbCreatedGroup gid newGroup.name

       UserChangedDeletion value -> void $ H.queryAll _groupName $ H.tell $ GroupName.DeletionEnabled value

       UserDeletedGroup gid -> do 
          s <- H.get
          if M.size s.groups > 1 then
            H.raise $ SbDeletedGroup gid $ getTabIdsOfGroup gid s.tabsToGroup
          else 
            void $ H.query _groupName gid $ H.tell $ GroupName.TriedToDeleteLastGroup

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

          deletedTab' <- H.query _tabs fromGroup $ H.request $ Tabs.TabDeleted tid
          case deletedTab' of 
               Just (Just (Tab tab)) -> 
                 void $ H.query _tabs toGroup $ H.tell 
                  $ Tabs.TabCreated $ Tab (tab { index = newIndexInGroup })
               _ -> pure unit

          H.raise $ SbMoveTab tid newTabIndex
          H.raise $ SbActivateTab tid
          H.raise $ SbChangeTabGroup tid (Just toGroup)

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

 
handleQuery :: forall a m. MonadEffect m => Query a -> H.HalogenM State Action Slots SidebarEvent m (Maybe a)
handleQuery = case _ of 
   TabsQuery q -> handleTabsQuery q

   InitializeGroups groups a -> do
      let newGroups = M.fromFoldable $ 
            A.mapWithIndex (\idx (GroupData g) -> Tuple g.groupId { name: g.name, pos: idx}) groups

      -- TODO: re-assign existing tabs to the new groups.
      H.modify_ \s ->
        if newGroups == s.groups then
          s
        else
          s { groups = newGroups }

      pure (Just a)


   InitialTabsWithGroup groups tabs a -> do
       -- Assign the tabs to their group and save the tabs positions
       s <- H.modify \s ->
         let 
             newGroups = 
               case groups of
                    [] -> initialGroup
                    newGroups' -> 
                      M.fromFoldable $ 
                        A.mapWithIndex 
                        (\idx (GroupData g) -> Tuple g.groupId { name: g.name, pos: idx})
                        newGroups'

             existingGroups = M.keys newGroups

             tabIdGroup = tabs <#> \(TabWithGroup (Tab t) gid) -> Tuple t.id gid
          in
             s { groups = newGroups, tabsToGroup = M.fromFoldable tabIdGroup, groupTabsPositions = tabIdGroup }

       -- Update the browser state to re-assign correctly all the tabs
       let 
           (groupsTupled :: Array (Tuple TabId GroupId)) = M.toUnfoldableUnordered s.tabsToGroup
           setGroups = groupsTupled <#>
              (\(Tuple tid gid) -> H.raise $ SbChangeTabGroup tid (Just gid)) 
       void $ sequence setGroups

       -- Initialize each child tabs component with its tabs
       let 
            tabsGroups = tabs <#> \(TabWithGroup tab@(Tab t) _) -> Tuple tab $ fromMaybe s.currentGroup (M.lookup t.id s.tabsToGroup)
            groupedTabs = A.groupBy (eqBy T.snd) (sortByKeyIndex T.snd tabsGroups)
       void $ traverse initializeGroup groupedTabs

       -- Activate the right tab and its group
       let activatedTab = tabsGroups # A.head <<< A.filter (\(Tuple (Tab t) _) -> t.active)
       activatedTab # maybe (pure unit) \(Tuple (Tab t) gid) -> do
         void $ tellChild gid $ Tabs.TabActivated Nothing t.id
         handleAction $ UserSelectedGroup gid

       pure (Just a)

      where 
            initializeGroup :: forall act. NonEmptyArray (Tuple Tab GroupId) -> H.HalogenM State act Slots SidebarEvent m Unit
            initializeGroup groupedTabs = 
              let 
                  gid = T.snd $ NonEmptyArray.head groupedTabs
              in 
                  void $ tellChild gid $ Tabs.InitialTabList $ A.fromFoldable $ T.fst <$> groupedTabs

   TabCreated (Tab tab) groupId a -> do 
       s <- H.get

       let tabGroupId = fromMaybe s.currentGroup groupId

           newGroupTabsPositions = 
             fromMaybe s.groupTabsPositions 
             $ A.insertAt tab.index (Tuple tab.id tabGroupId) s.groupTabsPositions

           inGroupPosition = getPositionTabInGroup tab.index tabGroupId newGroupTabsPositions 

           newTab = Tab $ tab { index = inGroupPosition }

       newS <- H.modify \state -> 
         state 
         { tabsToGroup = M.insert tab.id tabGroupId s.tabsToGroup 
         , groupTabsPositions = newGroupTabsPositions
         }

       void $ tellChild tabGroupId $ Tabs.TabCreated newTab
       H.raise $ SbChangeTabGroup tab.id (Just tabGroupId)

       -- XXX: Temporary fix because Background.onTabCreated launches an async
       -- computation to create a tab instead of doing it synchronously, which
       -- makes the tab activation trigger *before* the tab creation.
       if tab.active then 
         void $ handleTabsQuery $ Tabs.TabActivated Nothing tab.id Nothing 
       else 
         pure unit
       pure (Just a)

   GroupDeleted gid currentTid a -> do 
      H.modify_ \s -> 
        let 
            currentGroup = fromMaybe s.currentGroup $ currentTid >>= (flip M.lookup s.tabsToGroup)
         in
            s { groups = M.delete gid s.groups, currentGroup = currentGroup }
      pure $ Just a


handleTabsQuery :: forall act a m. MonadEffect m => Tabs.Query a -> H.HalogenM State act Slots SidebarEvent m (Maybe a)
handleTabsQuery = case _ of

    Tabs.InitialTabList tabs a -> pure $ Just a

    -- TODO: log an error, this shouldn't happen
    Tabs.TabCreated tab a -> pure $ Just a

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
         void $ H.query _tabs gid $ H.request $ Tabs.TabDeleted tid
       pure (Just (reply Nothing))

    Tabs.TabActivated prevTid' tid a -> do 
       for_ prevTid' \prevTid ->
         doOnTabGroup prevTid \gid -> 
           void $ tellChild gid $ Tabs.TabActivated prevTid' tid

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
       handleTabsQuery $ Tabs.TabDeleted tid \_ -> a

    Tabs.TabAttached tab a -> do 
       handleTabsQuery $ Tabs.TabCreated tab a

  where 
    doOnTabGroup 
      :: TabId 
      -> (GroupId -> H.HalogenM State act Slots SidebarEvent m Unit) 
      -> H.HalogenM State act Slots SidebarEvent m Unit
    doOnTabGroup tabId f = do
      { tabsToGroup } <- H.get
      case M.lookup tabId tabsToGroup of 
           Just groupId -> f groupId
           Nothing -> pure unit



tellChild :: forall act m. GroupId -> (H.Tell Tabs.Query) -> H.HalogenM State act Slots SidebarEvent m (Maybe Unit)
tellChild gid q = H.query _tabs gid $ H.tell q

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

-- | Obtain the window index of the last tab of a group.
lastWinTabIndexInGroup 
  :: GroupId
  -> Array (Tuple TabId GroupId)
  -> Maybe Int
lastWinTabIndexInGroup gid = 
  A.mapWithIndex (Tuple)
    >>> A.filter (T.snd >>> T.snd >>> (==) gid)
    >>> map T.fst
    >>> A.last

findNextGroupId :: S.Set GroupId -> GroupId
findNextGroupId values = 
  let GroupId(maxValue) = NES.max (NES.cons (GroupId 0) values)
   in GroupId(maxValue + 1)

createGroup :: (Maybe GroupId) -> State -> Tuple GroupId Group
createGroup mGid s =
  let 
      gid = fromMaybe' (\_ -> findNextGroupId $ M.keys s.groups) mGid
  in
    Tuple gid { name: "new group", pos: M.size s.groups }

insertGroup :: GroupId -> Group -> State -> State
insertGroup gid group s = s { groups = M.insert gid group s.groups }

