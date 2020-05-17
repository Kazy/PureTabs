module Sortable (Sortable, Options, Event, MoveEvent, PullMode, create) where

import Control.Alt ((<$>))
import Control.Alternative (pure)
import Control.Bind ((>>=))
import Control.Category ((<<<), (>>>))
import Control.Monad.Except (mapExcept, runExcept, throwError)
import Data.Array (intercalate)
import Data.Boolean (otherwise)
import Data.BooleanAlgebra ((||))
import Data.Either (Either(..))
import Data.Function (($))
import Data.Function.Uncurried (Fn3, runFn3)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Traversable (traverse)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Exception (throw)
import Foreign (F, Foreign, ForeignError(..), fail, isNull, isUndefined, readInt, readNull, readNullOrUndefined, readNumber, renderForeignError, tagOf, unsafeFromForeign)
import Foreign.Index ((!))
import Heterogeneous.Mapping (class MappingWithIndex)
import Prelude (bind)
import Prim.Row (class Union, class Cons) as Row
import Web.HTML (HTMLElement)
import Web.HTML.Event.DataTransfer (DataTransfer)
import Web.HTML.HTMLElement (DOMRect)

foreign import data Sortable :: Type

foreign import isTrue :: Foreign -> Boolean

foreign import isFalse :: Foreign -> Boolean

foreign import isClone :: Foreign -> Boolean

data PullMode
  = Clone
  | Bool Boolean
  | NotDefined

readPullMode :: Foreign -> F PullMode
readPullMode value
  | isNull value || isUndefined value = pure NotDefined
  | isTrue value = pure (Bool true)
  | isFalse value = pure (Bool false)
  | isClone value = pure Clone
  | otherwise = fail $ TypeMismatch "PullMode" (tagOf value)

type Event
  = { to :: HTMLElement
    , from :: HTMLElement
    , item :: HTMLElement
    , clone :: HTMLElement
    , oldIndex :: Maybe Int
    , newIndex :: Maybe Int
    , oldDraggableIndex :: Maybe Int
    , newDraggableIndex :: Maybe Int
    , pullMode :: PullMode
    }

{-- foreign import data ForeignEvent :: Type --}
readEvent :: Foreign -> F Event
readEvent value = do
  to <- value ! "to" >>= (pure <<< unsafeFromForeign)
  from <- value ! "from" >>= (pure <<< unsafeFromForeign)
  item <- value ! "item" >>= (pure <<< unsafeFromForeign)
  clone <- value ! "clone" >>= (pure <<< unsafeFromForeign)
  oldIndex <- value ! "oldIndex" >>= readNullOrUndefined >>= traverse readInt
  newIndex <- value ! "newIndex" >>= readNullOrUndefined >>= traverse readInt
  oldDraggableIndex <- value ! "oldDraggableIndex" >>= readNullOrUndefined >>= traverse readInt
  newDraggableIndex <- value ! "newDraggableIndex" >>= readNullOrUndefined >>= traverse readInt
  pullMode <- value ! "pullMode" >>= readPullMode
  pure { to, from, item, clone, oldIndex, newIndex, oldDraggableIndex, newDraggableIndex, pullMode }

type MoveEvent
  = { to :: HTMLElement
    , from :: HTMLElement
    , dragged :: HTMLElement
    , draggedRect :: DOMRect
    , related :: HTMLElement
    , relatedRect :: DOMRect
    , willInsertAfter :: Boolean
    }

type Options
  = ( group :: String
    , sort :: Boolean
    , delay :: Int
    , delayOnTouchOnly :: Boolean
    , touchStartThreshold :: Int
    , disabled :: Boolean
    , {-- store ::  --} animation :: Int
    , easing :: String
    , handle :: String
    , filter :: String
    , preventOnFilter :: Boolean
    , draggable :: String
    , dataIdAttr :: String
    , ghostClass :: String
    , chosenClass :: String
    , dragClass :: String
    , swapThreshold :: Int
    , invertSwap :: Boolean
    , invertedSwapThreshold :: Int
    , direction :: String
    , forceFallback :: Boolean
    , fallbackClass :: String
    , fallbackOnBody :: Boolean
    , fallbackTolerance :: Int
    , dragoverBubble :: Boolean
    , removeCloneOnHide :: Boolean
    , emptyInsertThreshold :: Number
    {-- , setData :: DataTransfer -> HTMLElement -> Effect Unit --}
    , onChoose :: Event -> Effect Unit
    , onUnchoose :: Event -> Effect Unit
    , onStart :: Event -> Effect Unit
    , onEnd :: Event -> Effect Unit
    , onAdd :: Event -> Effect Unit
    , onUpdate :: Event -> Effect Unit
    , onSort :: Event -> Effect Unit
    , onRemove :: Event -> Effect Unit
    , onFilter :: Event -> Effect Unit
    , onMove :: MoveEvent -> Effect Unit
    , onClone :: Event -> Effect Unit
    , onChange :: Event -> Effect Unit
    )

foreign import create' ::
  forall given.
  Fn3
    { | given }
    HTMLElement
    ((Event -> Effect Unit) -> (Foreign -> Effect Unit))
    (Effect Sortable)

parseEvent :: (Event -> Effect Unit) -> (Foreign -> Effect Unit)
parseEvent f = wrappedF
  where
  wrappedF :: Foreign -> Effect Unit
  wrappedF =
    readEvent >>> runExcept
      >>> ( case _ of
            Left err -> throw $ formatErr err
            Right event -> f event
        )

  formatErr :: NonEmptyList ForeignError -> String
  formatErr err = intercalate ", " $ renderForeignError <$> err

create ::
  forall given trash.
  Row.Union given trash Options =>
  { | given } ->
  HTMLElement ->
  Effect Sortable
create options elem = runFn3 create' options elem parseEvent
