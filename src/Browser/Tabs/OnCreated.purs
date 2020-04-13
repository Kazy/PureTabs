module Browser.Tabs.OnCreated (addListener, removeListener) where

import Browser.Tabs (Tab)
import Browser.Utils (Listener, UnregisteredListener, mkListenerOne)
import Control.Alt (map)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Array (intercalate)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Exception (throw)
import Foreign (Foreign, renderForeignError)
import Foreign.Generic (defaultOptions, genericDecode)
import Prelude (Unit, bind, ($))

foreign import addListenerImpl :: (Listener Foreign) -> Effect Unit

addListener :: (UnregisteredListener Tab) -> Effect Unit
addListener listener = do
  lst <- mkListenerOne foreignListener
  addListenerImpl lst
  where
  foreignListener :: UnregisteredListener Foreign
  foreignListener f = case runExcept $ genericDecode (defaultOptions { unwrapSingleConstructors = true }) f of
    Left err -> throw $ intercalate ", " (map renderForeignError err)
    Right val -> listener val

foreign import removeListener :: (Listener Tab) -> Effect Unit
