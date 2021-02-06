module Sidebar.Utils (whenC, moveElem) where 

import Data.Array ((!!), insertAt, deleteAt) as A
import Data.Maybe (Maybe)
import Halogen (ClassName(..))
import Prelude (bind, (>=>))


whenC :: Boolean -> ClassName -> ClassName
whenC b c = if b then c else ClassName ""

moveElem :: forall a. Int -> Int -> Array a -> Maybe (Array a)
moveElem from to arr = do
  elem <- arr A.!! from
  (A.deleteAt from >=> A.insertAt to elem) arr
