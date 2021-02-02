module Sidebar.Component.GroupName (component, NewName) where


import Control.Monad.Free (liftF)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML (span, text)
import Halogen.HTML as HH
import Halogen.HTML.Core (ref)
import Halogen.HTML.Events (onChange, onDoubleClick, onInput, onKeyUp)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (autofocus, ref)
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookF(..), OutputToken, getHTMLElementRef, put, query, raise, subscribe', useTickEffect)
import Halogen.Hooks as Hooks
import Halogen.Query (getHTMLElementRef)
import Halogen.Query as HQ
import Halogen.Query.Input as HQI
import Prelude (bind, const, discard, flap, liftM1, map, otherwise, pure, unit, ($), (<$>), (=<<), (==), (>=>), (>>>))
import Web.Event.Event (target)
import Web.Event.Event as E
import Web.Event.EventTarget (EventTarget)
import Web.Event.EventTarget as ET
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.HTML.HTMLElement (focus) as Web
import Web.UIEvent.InputEvent (InputEvent, fromEvent)
import Web.UIEvent.InputEvent as IE
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

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
            Just do 
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
             case elem of 
                  Just elem -> liftEffect $ Web.focus elem
                  _ -> pure unit
        ] [HH.text chars]
