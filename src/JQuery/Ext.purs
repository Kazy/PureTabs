module JQuery.Ext (after) where

import Prelude (Unit)
import Effect (Effect)
import JQuery (JQuery)

foreign import after :: JQuery -> JQuery -> Effect Unit
