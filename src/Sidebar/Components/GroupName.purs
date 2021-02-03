module Sidebar.Component.GroupName (component, NewName) where


import Control.Category ((<<<))
import Data.Maybe (Maybe(..), maybe)
import Data.String.CodeUnits (length)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Query.Input as HQI
import Prelude (bind, discard, otherwise, pure, unit, ($), (==))
import Web.Event.Event as E
import Web.Event.EventTarget as ET
import Web.HTML.HTMLElement (focus) as Web
import Web.UIEvent.KeyboardEvent as KE

type NewName = String

foreign import targetValue :: ET.EventTarget -> String

component
  :: forall unusedQuery anyMonad
   . MonadAff anyMonad
  => H.Component HH.HTML unusedQuery String NewName anyMonad
component = Hooks.component \rec name -> Hooks.do 
  isRenaming /\ isRenamingIdx <- Hooks.useState false 
  initialName /\ initialNameIdx <- Hooks.useState name 
  chars /\ charsIdx <- Hooks.useState name

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
                 Hooks.raise rec.outputToken chars
        | KE.key keyEvent == "Escape" = 
          Just do 
             Hooks.put charsIdx initialName
             Hooks.put isRenamingIdx false
        | otherwise = Nothing

      onInput input = do 
         target <- E.target input
         let value = targetValue target
         Just $ Hooks.put charsIdx value

  Hooks.pure $
      if isRenaming then 
        HH.input [ HP.type_ HP.InputText, HP.value chars, HE.onKeyUp onKeyEvent, HE.onInput onInput, HP.ref (HQI.RefLabel "input") ] 
      else 
        HH.span [ 
          HE.onDoubleClick \_ -> Just $ do 
             Hooks.put isRenamingIdx true 
             elem <- Hooks.getHTMLElementRef (HQI.RefLabel "input")
             maybe (pure unit) (liftEffect <<< Web.focus) elem
        ] [HH.text chars]
