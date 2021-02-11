module PureTabs.Sidebar.Component.TopMenu (component, TopMenuAction(..), Slot) where 

import Prelude (($), bind, not)

import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks


type Slot a = forall q. H.Slot q TopMenuAction a


data TopMenuAction
  = CreateGroup
  | ChangedDeletion Boolean


component 
  :: forall unusedQuery unusedInput anyMonad
   . H.Component HH.HTML unusedQuery unusedInput TopMenuAction anyMonad
component = Hooks.component \rec _ -> Hooks.do
  isDeleting /\ isDeletingIdx <- Hooks.useState false

  let menuElem attrs text = HH.li attrs [ HH.text text ]

  Hooks.pure $ 
    HH.div [ HP.id_ "top-menu" ] [
      HH.ul [] [
        menuElem [HE.onClick \_ -> Just $ Hooks.raise rec.outputToken CreateGroup] "+", 
        menuElem [
          HE.onClick \_ -> Just $ do
             isNowDeleting <- Hooks.modify isDeletingIdx (not)
             Hooks.raise rec.outputToken $ ChangedDeletion isNowDeleting
       ] if isDeleting then "✓" else "-"
      ]
    ]

