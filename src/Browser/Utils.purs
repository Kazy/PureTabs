module Browser.Utils
  ( UnregisteredListener
  , UnregisteredListener2
  , UnregisteredListener3
  , Listener
  , Listener2
  , Listener3
  , mkListenerUnit
  , mkListenerOne
  , mkListenerTwo
  , mkListenerThree
  , unwrapForeign
  , unsafeLog
  , unsafeLog'
  , eqBy
  , sortByKeyIndex
  ) where

import Control.Alt (map)
import Control.Alternative (pure)
import Control.Monad.Except (runExcept)
import Data.Array as A
import Data.Either (Either(..))
import Data.Eq (class Eq, (==))
import Data.Foldable (fold)
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.Ord (class Ord)
import Data.Tuple as T
import Effect (Effect)
import Effect.Exception (throw)
import Foreign (Foreign, renderForeignError)
import Foreign.Generic (class GenericDecode, defaultOptions, genericDecode)
import Prelude (Unit, comparing, (>>>))

type UnregisteredListener a
  = (a -> Effect Unit)

type UnregisteredListener2 a b
  = (a -> b -> Effect Unit)

type UnregisteredListener3 a b c
  = (a -> b -> c -> Effect Unit)

newtype Listener a
  = Listener (UnregisteredListener a)

newtype Listener2 a b
  = Listener2 (UnregisteredListener2 a b)

newtype Listener3 a b c
  = Listener3 (UnregisteredListener3 a b c)

foreign import mkListenerUnit :: (Effect Unit) -> Effect (Listener Unit)

foreign import mkListenerOne :: forall a. (UnregisteredListener a) -> Effect (Listener a)

foreign import mkListenerTwo :: forall a b. (UnregisteredListener2 a b) -> Effect (Listener2 a b)

foreign import mkListenerThree :: forall a b c. (UnregisteredListener3 a b c) -> Effect (Listener3 a b c)

unwrapForeign :: forall a rep. Generic a rep => GenericDecode rep => Foreign -> Effect a
unwrapForeign d = case runExcept
    $ genericDecode (defaultOptions { unwrapSingleConstructors = true }) d of
  Left err -> throw $ A.intercalate ", " (map renderForeignError err)
  Right val -> pure val

foreign import unsafeLog' :: forall a. a
foreign import unsafeLog :: forall a. a -> Effect Unit

-- | Given a mapping function from a to b, where Eq is defined for b, return a
-- | comparison function.
eqBy :: forall a b. Eq b => (a -> b) -> (a -> a -> Boolean)
eqBy f = \a b -> f a == f b

-- | Given a mapping function from a to b where Ord is defined for b, sort the
-- | array by the mapping function, tie-breaking using the index.
sortByKeyIndex :: forall a b. Ord b => (a -> b) -> Array a -> Array a
sortByKeyIndex cmp = A.mapWithIndex T.Tuple >>> A.sortBy compareKey >>> map T.snd
  where compareGiven = comparing (T.snd >>> cmp)
        compareIdx = comparing T.fst
        compareKey = fold [compareGiven, compareIdx]
