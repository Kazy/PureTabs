module JQuery.Ext (after, prepend, getHtmlElem) where

import Effect (Effect)
import JQuery (JQuery)
import Prelude (Unit)
import Web.HTML (HTMLElement)

foreign import after :: JQuery -> JQuery -> Effect Unit
foreign import prepend :: JQuery -> JQuery -> Effect Unit
-- XXX: should probably be a maybe ?
foreign import getHtmlElem :: JQuery -> Effect HTMLElement
