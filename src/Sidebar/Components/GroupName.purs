module PureTabs.Sidebar.Component.GroupName (component, Output(..), Query(..), Slot) where


import Control.Category ((<<<))
import Data.Maybe (Maybe(..), maybe)
import Data.String.CodeUnits (length)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Query.Input as HQI
import Prelude (bind, discard, otherwise, pure, unit, ($), (==))
import Sidebar.Utils (whenC)
import Web.Event.Event as E
import Web.Event.EventTarget as ET
import Web.HTML.HTMLElement (focus) as Web
import Web.UIEvent.KeyboardEvent as KE

type Slot a = H.Slot Query Output a

data Output 
  = NewName String
  | DeleteGroup

data Query a
  = DeletionEnabled Boolean a
  | TriedToDeleteLastGroup a

foreign import targetValue :: ET.EventTarget -> String

component
  :: forall m
   . MonadAff m
  => H.Component HH.HTML Query String Output m
component = Hooks.component \rec name -> Hooks.do 
  isRenaming /\ isRenamingIdx <- Hooks.useState false 
  initialName /\ initialNameIdx <- Hooks.useState name 
  chars /\ charsIdx <- Hooks.useState name

  deletionEnabled /\ deletionEnabledIdx <- Hooks.useState false
  triedToDelete /\ triedToDeleteIdx <- Hooks.useState false

  Hooks.useQuery rec.queryToken case _ of 

    DeletionEnabled value a -> do 
       Hooks.put deletionEnabledIdx value
       pure Nothing

    TriedToDeleteLastGroup a -> do
       Hooks.put triedToDeleteIdx true
       -- TODO: Add a debounce for setting triedToDelete to false. This will
       -- avoid the animation getting cancelled if we click multiple times too
       -- fast on the button.
       liftAff $ Aff.delay $ Milliseconds 800.0
       Hooks.put triedToDeleteIdx false
       pure Nothing

  let 
      onKeyEvent keyEvent 
        | KE.key keyEvent == "Enter" = 
            Just $ case (length chars) of 
              0 -> do 
                 Hooks.put isRenamingIdx false
                 Hooks.put charsIdx initialName
              _ -> do
                 Hooks.put isRenamingIdx false 
                 Hooks.put initialNameIdx chars
                 Hooks.raise rec.outputToken $ NewName chars
        | KE.key keyEvent == "Escape" = 
          Just do 
             Hooks.put charsIdx initialName
             Hooks.put isRenamingIdx false
        | otherwise = Nothing

      onInput input = do 
         target <- E.target input
         let value = targetValue target
         Just $ Hooks.put charsIdx value

      groupName = HH.text chars
      node = 
        if deletionEnabled then 
          [HH.span [
            HP.class_ $ H.ClassName "group-deletion-button"
            , HE.onClick \_ -> Just $ Hooks.raise rec.outputToken DeleteGroup
          ] [HH.text "✖"], groupName]
        else
          [groupName]


  Hooks.pure $
      if isRenaming then 
        HH.input [ HP.type_ HP.InputText, HP.value chars, HE.onKeyUp onKeyEvent, HE.onInput onInput, HP.ref (HQI.RefLabel "input") ] 
      else 
        HH.span [ 
          HP.class_ $ whenC triedToDelete (H.ClassName "shake-animation")
          , HE.onDoubleClick \_ -> if deletionEnabled then Nothing else Just $ do 
             Hooks.put isRenamingIdx true 
             elem <- Hooks.getHTMLElementRef (HQI.RefLabel "input")
             maybe (pure unit) (liftEffect <<< Web.focus) elem
        ] node
