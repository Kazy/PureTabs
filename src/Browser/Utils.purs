module Browser.Utils
  ( UnregisteredListener
  , UnregisteredListener2
  , Listener
  , Listener2
  , mkListenerUnit
  , mkListenerOne
  , mkListenerTwo
  , unwrapForeign
  ) where

import Control.Alt (map)
import Control.Alternative (pure)
import Control.Monad.Except (runExcept)
import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Effect.Exception (throw)
import Foreign (Foreign, renderForeignError)
import Foreign.Generic (class GenericDecode, defaultOptions, genericDecode)
import Prelude (Unit)

type UnregisteredListener a
  = (a -> Effect Unit)

type UnregisteredListener2 a b
  = (a -> b -> Effect Unit)

newtype Listener a
  = Listener (UnregisteredListener a)

newtype Listener2 a b
  = Listener2 (UnregisteredListener2 a b)

foreign import mkListenerUnit :: (Effect Unit) -> Effect (Listener Unit)

foreign import mkListenerOne :: forall a. (UnregisteredListener a) -> Effect (Listener a)

foreign import mkListenerTwo :: forall a b. (UnregisteredListener2 a b) -> Effect (Listener2 a b)


unwrapForeign :: forall a rep. Generic a rep => GenericDecode rep => Foreign -> Effect a
unwrapForeign d = case runExcept $ genericDecode (defaultOptions { unwrapSingleConstructors = true }) d of
  Left err -> throw $ intercalate ", " (map renderForeignError err)
  Right val -> pure val
