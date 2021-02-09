module PureTabs.Browser.Dom.Element (scrollIntoView) where

import Prelude

import Effect (Effect)
import Web.DOM.Element (Element)



foreign import scrollIntoView :: Element -> Effect Unit

