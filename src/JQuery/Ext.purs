module JQuery.Ext (after, prepend) where

import Prelude (Unit)
import Effect (Effect)
import JQuery (JQuery)

foreign import after :: JQuery -> JQuery -> Effect Unit
foreign import prepend :: JQuery -> JQuery -> Effect Unit
