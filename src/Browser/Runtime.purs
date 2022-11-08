module Browser.Runtime (
  Port,
  connect,
  onConnectAddListener,
  portOnDisconnect,
  postMessage,
  postMessageJson,
  onMessageAddListener,
  onMessageJsonAddListener,
  onMessageRemoveListener,
  portHasError
) where

import Browser.Utils (mkListenerOne, Listener, UnregisteredListener)
import Control.Alt (map)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Monoid ((<>))
import Effect (Effect)
import Effect.Console (error)
import Foreign (renderForeignError)
import Foreign.Generic (class GenericEncode, class GenericDecode, defaultOptions, genericEncodeJSON, genericDecodeJSON)
import Prelude (Unit, ($), bind, discard, pure)
  
foreign import data Port :: Type

foreign import portEquality :: Port -> Port -> Boolean

instance eqPort :: Eq Port where
  eq = portEquality

foreign import connect :: Effect Port

foreign import onConnectAddListener :: Listener Port -> Effect Unit

foreign import postMessage :: forall a. Port -> a -> Effect Unit

postMessageJson :: forall a rep. Generic a rep => GenericEncode rep => Port -> a -> Effect Unit
postMessageJson port d = postMessage port $ genericEncodeJSON (defaultOptions { unwrapSingleConstructors = true}) d

foreign import portOnDisconnect :: Port -> Listener Unit -> Effect Unit

foreign import onMessageAddListener :: forall a. Port -> Listener a -> Effect Unit

foreign import portHasError :: Port -> Effect Boolean

onMessageJsonAddListener :: forall a rep. Generic a rep => GenericDecode rep => Port -> UnregisteredListener a -> Effect (Listener String)
onMessageJsonAddListener port f = do 
  jsonLst <- mkListenerOne listener
  onMessageAddListener port jsonLst
  pure jsonLst

  where
        listener msg = case runExcept (genericDecodeJSON (defaultOptions { unwrapSingleConstructors = true}) msg :: _ a) of 
                 Left err -> do 
                    error $ "error while trying to parse message: " <> intercalate ", " (map renderForeignError err)
                    error $ "message was " <> msg
                 Right d -> f d

foreign import onMessageRemoveListener :: forall a. Port -> Listener a -> Effect Unit
