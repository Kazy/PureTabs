module PureTabs.Sidebar.Bar where

import Browser.Tabs (Tab(..), TabId)
import Control.Alternative (pure, (<$>))
import Control.Bind (bind, discard, void, (<#>))
import Data.Function (($))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), uncurry)
import Data.Unit (Unit, unit)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Prelude (class Eq, class Ord, (<<<))
import PureTabs.Model (SidebarEvent)
import PureTabs.Sidebar.Tabs (Output(..))
import PureTabs.Sidebar.Tabs as Tabs

newtype GroupId
  = GroupId Int

derive instance eqGroupId :: Eq GroupId

derive instance ordGroupId :: Ord GroupId

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
  | HandleTabsOutput GroupId Tabs.Output

initialState :: forall i. i -> State
initialState _ =
  let
    firstGroupId = GroupId 0
  in
    { groups: M.singleton firstGroupId { name: "main", pos: 0 }, tabsToGroup: M.empty, currentGroup: firstGroupId }

type Slots
  = ( tabs :: H.Slot Tabs.Query Tabs.Output GroupId )

_childLabel :: SProxy "tabs"
_childLabel = (SProxy :: _ "tabs")

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
  render state = HH.div_ [
    HH.div_ $ (uncurry renderTab) <$> (M.toUnfoldable state.groups)
  ]

  renderTab :: GroupId -> Group -> H.ComponentHTML Action Slots m
  renderTab groupId group = HH.slot _childLabel groupId Tabs.component unit (Just <<< (HandleTabsOutput groupId))

  handleAction :: Action -> H.HalogenM State Action Slots SidebarEvent m Unit
  handleAction = case _ of
    UserSelectedGroup gid -> pure unit
    HandleTabsOutput gid event -> case event of
      TabsSidebarAction sbEvent -> H.raise sbEvent

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
        tellChild gid q = H.query _childLabel gid $ H.tell q
