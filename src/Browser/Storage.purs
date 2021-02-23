module PureTabs.Browser.Storage (storageLocalGet, storageLocalSet) where

import Prelude

import Browser.Utils (unsafeLog)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (runExcept)
import Control.Promise (Promise, toAffE)
import Data.Either (Either(..), hush)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Effect.Aff (Aff, throwError)
import Effect.Aff.Compat (EffectFn1, runEffectFn1)
import Effect.Class (liftEffect)
import Effect.Class.Console (error) as Log
import Effect.Class.Console (log)
import Effect.Exception (error)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign (Foreign, renderForeignError)
import Foreign.Generic (class GenericDecode, defaultOptions, genericDecode)

foreign import storageLocalGetImpl :: EffectFn1 String  (Promise Foreign)

storageLocalGet' :: forall r rep. Generic r rep => GenericDecode rep => String -> Aff r
storageLocalGet' keys = do 
  msg <- toAffE $ runEffectFn1 storageLocalGetImpl keys
  case runExcept (genericDecode (defaultOptions { unwrapSingleConstructors = true}) msg :: _ r) of
       Left err -> do 
          Log.error $ "error while trying to parse message: " <> intercalate ", " (map renderForeignError err)
          throwError $ error "couldn't decode msg"
       Right resp -> pure resp

-- TODO: differentiate between missing key and decoding error
storageLocalGet :: forall r rep. Generic r rep => GenericDecode rep => String -> Aff (Maybe r)
storageLocalGet key = hush <$> (try $ storageLocalGet' key)

foreign import storageLocalSetImpl
  :: forall r. EffectFn2 String r (Promise Unit)

storageLocalSet :: forall r. String -> r -> Aff Unit
storageLocalSet key value = toAffE $ runEffectFn2 storageLocalSetImpl key value
