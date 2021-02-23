module PureTabs.Browser.Sessions where

import Prelude

import Browser.Tabs (TabId(..), WindowId(..))
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Except (runExcept)
import Control.Promise (Promise, toAffE)
import Data.Either (Either(..), hush)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console as Log
import Effect.Exception (error)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, runEffectFn2, runEffectFn3, runEffectFn4)
import Foreign (renderForeignError)
import Foreign.Generic (class GenericDecode, defaultOptions, genericDecode)

foreign import setTabValueImpl 
  :: forall r. EffectFn3 Number String r (Promise Unit)

-- | Set a value from a tab.
setTabValue 
  :: forall r
   . TabId
  -> String
  -> r
  -> Aff Unit
setTabValue (TabId tid) key value = toAffE $ runEffectFn3 setTabValueImpl tid key value

foreign import removeTabValueImpl
  :: EffectFn2 Number String (Promise Unit)

removeTabValue
  :: TabId
  -> String
  -> Aff Unit
removeTabValue (TabId tid) key = toAffE $ runEffectFn2 removeTabValueImpl tid key

foreign import getTabValueImpl
  :: forall r. EffectFn4 (r -> Maybe r) (Maybe r) Number String (Promise (Maybe r))

-- | Get a value from a tab.
getTabValue
  :: forall r
   . TabId
  -> String
  -> Aff (Maybe r)
getTabValue (TabId tid) key = toAffE $ runEffectFn4 getTabValueImpl Just Nothing tid key


foreign import setWindowValueImpl 
  :: forall r. EffectFn3 Number String r (Promise Unit)

-- | Set a value for a window. The type `r` should be represented the same way
-- | in Purescript and in Javascript (so primitive types and newtypes of
-- | primitive types only).
--
-- TODO: use GenericEncode to properly encode the data we're setting.
setWindowValue 
  :: forall r
   . WindowId
  -> String
  -> r
  -> Aff Unit
setWindowValue (WindowId winId) key value = toAffE $ runEffectFn3 setWindowValueImpl winId key value

foreign import removeWindowValueImpl
  :: EffectFn2 Number String (Promise Unit)

removeWindowValue
  :: WindowId
  -> String
  -> Aff Unit
removeWindowValue (WindowId winId) key = toAffE $ runEffectFn2 removeWindowValueImpl winId key

foreign import getWindowValueImpl :: forall r. EffectFn2 Number String (Promise r)

-- | Get the value from a window. Throw an error if we couldn't decode it or we couldn't decode it.
getWindowValue'
  :: forall r rep
   . Generic r rep
  => GenericDecode rep
  => WindowId
  -> String
  -> Aff r
getWindowValue' (WindowId winId) key = do 
  content <- toAffE $ runEffectFn2 getWindowValueImpl winId key
  case runExcept (genericDecode (defaultOptions { unwrapSingleConstructors = true}) content :: _ r) of
       Left err -> do 
          Log.error $ "error while trying to getWindowValue of " <> key <> ": " <> intercalate ", " (map renderForeignError err)
          throwError $ error "couldn't decode msg"
       Right resp -> pure resp

-- | Get the value from a window, returning Nothing if it doesn't exist or we couldn't decode it.
getWindowValue
  :: forall r rep
   . Generic r rep
  => GenericDecode rep
  => WindowId
  -> String
  -> Aff (Maybe r)
getWindowValue winId key = hush <$> (try $ getWindowValue' winId key)
