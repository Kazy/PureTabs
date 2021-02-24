module PureTabs.Model.TabWithGroup where

import Browser.Tabs (Tab)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Show (class Show)
import PureTabs.Model.Group (GroupId)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)

data TabWithGroup
  = TabWithGroup Tab GroupId

derive instance genTabWithGroup :: Generic TabWithGroup _

instance showTabWithGroup :: Show TabWithGroup where 
  show = genericShow

instance encodeTabWithGroup :: Encode TabWithGroup where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

instance decodeTabWithGroup :: Decode TabWithGroup where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x
