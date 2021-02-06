module PureTabs.Sidebar.Bar where

import Browser.Tabs (Tab(..), TabId)
import Control.Alternative (pure)
import Control.Bind (bind, discard, map, void, (*>), (<#>))
import Data.Array ((:))
import Data.Array as A
import Data.Function (($))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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
    }

data Action
  = UserSelectedGroup GroupId
  | UserRenameGroup GroupId String
  | UserCreatedGroup
  | UserDeletedGroup GroupId
  | HandleTabsOutput GroupId Tabs.Output

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
        menuElem attrs text = HH.li attrs [ HH.text text]

        topMenu = HH.div [ HP.id_ "bar-menu" ] [
          HH.ul [] [menuElem [HE.onClick \_ -> Just UserCreatedGroup] "+", menuElem [] "-"]
        ]

        barListGroup = HH.div [ HP.id_ "bar-list" ] [HH.ul [ HP.id_ "bar-list-group"] $ 
          (M.toUnfoldable state.groups) <#> \(Tuple gid g) -> renderGroup gid (gid == state.currentGroup) g
        ]

        tabsDivs = (S.toUnfoldable $ (M.keys state.groups)) <#> 
          (\gid -> HH.div [
            HP.classes [(H.ClassName "bar-tabs"), whenC (gid == state.currentGroup) (H.ClassName "bar-tabs-active")] 
          ] [renderGroupTabs gid])
    
     in
        HH.div [ HP.id_ "bar" ] $ topMenu : barListGroup : tabsDivs 

  renderGroupTabs :: GroupId -> H.ComponentHTML Action Slots m
  renderGroupTabs groupId = HH.slot _tab groupId Tabs.component unit (Just <<< (HandleTabsOutput groupId))

  renderGroup :: GroupId -> Boolean -> Group -> H.ComponentHTML Action Slots m
  renderGroup groupId isActive group =  
    HH.li [ 
      HP.classes [(H.ClassName "group-name"), whenC isActive (H.ClassName "active-group")]
      , HE.onClick (\_ -> Just (UserSelectedGroup groupId))
    ] [ HH.slot _groupName groupId GroupName.component group.name (\newName -> Just (UserRenameGroup groupId newName))] 

  handleAction :: Action -> H.HalogenM State Action Slots SidebarEvent m Unit
  handleAction = 
    case _ of

         UserSelectedGroup gid -> H.modify_ _ { currentGroup = gid }

         UserRenameGroup gid newName -> 
            H.modify_ \s -> s { groups = M.update (\g -> Just $ g { name = newName }) gid s.groups }

         UserCreatedGroup -> do
           H.modify_ \s -> s { groups = M.insert (findNextGroupId $ M.keys s.groups) { name: "new group", pos: M.size s.groups } s.groups }

         UserDeletedGroup gid -> pure unit

         HandleTabsOutput gid (TabsSidebarAction sbEvent) -> 
           case sbEvent of 
                -- Important: we ask Firefox to do the move, but we don't
                -- perform it ourselves.  This means we don't update the state.
                -- We will get back a TabMoved event that will then be
                -- processed accordingly.
                SbMoveTab tid groupIndex -> do
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

                   maybe (pure unit) (\idx -> 
                     H.raise (SbMoveTab tid idx) 
                     *> (H.liftEffect $ 
                       log $ "sb: asking to move tab id " <> (show tid) 
                         <> " from " <> (show oldPosition) <> " to pos " <> (show idx) 
                         <> " (group index: " <> (show groupIndex) <> ", gid: " <> (show gid) <> ")"
                   )) newIndex

                _ -> H.raise sbEvent

    where
          findNextGroupId :: S.Set GroupId -> GroupId
          findNextGroupId values = 
            let GroupId(maxValue) = NES.max (NES.cons (GroupId 0) values)
             in GroupId(maxValue + 1)

  handleQuery :: forall act o a. Tabs.Query a -> H.HalogenM State act Slots o m (Maybe a)
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
       void $ tellChild s.currentGroup $ Tabs.InitialTabList tabs
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

    Tabs.TabDeleted tid a -> do 
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
         void $ tellChild gid $ Tabs.TabDeleted tid
       pure (Just a)

    Tabs.TabActivated oldTid tid a -> do 
       doOnTabGroup tid \gid -> do 
         H.modify_ (_ { currentGroup = gid})
         void $ tellChild gid $ Tabs.TabActivated oldTid tid
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
       handleQuery $ Tabs.TabDeleted tid a

    Tabs.TabAttached tab a -> do 
       handleQuery $ Tabs.TabCreated tab a

    Tabs.TabDeactivated tid a -> do 
       doOnTabGroup tid \gid -> do 
          void $ tellChild gid $ Tabs.TabDeactivated tid
       pure (Just a)

    where
        tellChild :: GroupId -> (H.Tell Tabs.Query) -> H.HalogenM State act Slots o m (Maybe Unit)
        tellChild gid q = H.query _tab gid $ H.tell q

        doOnTabGroup 
          :: TabId 
          -> (GroupId -> H.HalogenM State act Slots o m Unit) 
          -> H.HalogenM State act Slots o m Unit
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
