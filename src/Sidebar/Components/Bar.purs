module PureTabs.Sidebar.Bar where

import Browser.Tabs (Tab(..), TabId)
import Control.Alternative (pure)
import Control.Bind (bind, discard, void, (<#>))
import Data.Array ((:))
import Data.Function (($))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set (toUnfoldable, Set) as S
import Data.Set.NonEmpty (cons, max) as NES
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (class Eq, class Ord, class Show, show, (+), (<<<), (<>), (==))
import PureTabs.Model (SidebarEvent)
import PureTabs.Sidebar.Tabs (Output(..))
import PureTabs.Sidebar.Tabs as Tabs
import Sidebar.Component.GroupName as GroupName
import Sidebar.Utils (whenC)

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
         HandleTabsOutput gid (TabsSidebarAction sbEvent) -> H.raise sbEvent

    where
          findNextGroupId :: S.Set GroupId -> GroupId
          findNextGroupId values = 
            let GroupId(maxValue) = NES.max (NES.cons (GroupId 0) values)
             in GroupId(maxValue + 1)

  handleQuery :: forall act o a. Tabs.Query a -> H.HalogenM State act Slots o m (Maybe a)
  handleQuery = case _ of
    -- select the current group
    -- associate all the tab id to the current group
    -- send an action to the corresponding slot
    Tabs.InitialTabList tabs a -> do
       s <- H.modify (\s -> s { tabsToGroup = M.fromFoldable $ tabs <#> \(Tab t) -> Tuple t.id s.currentGroup })
       void $ tellChild s.currentGroup $ Tabs.InitialTabList tabs
       pure (Just a)
    Tabs.TabCreated (Tab t) a -> do 
       s <- H.modify (\s -> s { tabsToGroup = M.insert t.id s.currentGroup  s.tabsToGroup })
       void $ tellChild s.currentGroup $ Tabs.TabCreated (Tab t)
       pure (Just a)
    Tabs.TabDeleted tid a -> do 
       s <- H.modify (\s -> s { tabsToGroup = M.delete tid s.tabsToGroup })
       void $ tellChild s.currentGroup $ Tabs.TabDeleted tid
       pure (Just a)
    Tabs.TabActivated oldTid tid a -> do 
       s <- H.get
       void $ tellChild s.currentGroup $ Tabs.TabActivated oldTid tid
       pure (Just a)
    Tabs.TabMoved tid prev next a -> do 
       s <- H.get
       void $ tellChild s.currentGroup $ Tabs.TabMoved tid prev next
       pure (Just a)
    Tabs.TabInfoChanged tid cinfo a -> do 
       s <- H.get
       void $ tellChild s.currentGroup $ Tabs.TabInfoChanged tid cinfo
       pure (Just a)

    where
        tellChild :: GroupId -> (H.Tell Tabs.Query) -> H.HalogenM State act Slots o m (Maybe Unit)
        tellChild gid q = H.query _tab gid $ H.tell q
