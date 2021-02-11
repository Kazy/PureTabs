module PureTabs.Model.Group (GroupId(..)) where


import Prelude (class Eq, class Ord, class Show, (<>), show)
import Data.Generic.Rep (class Generic)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)

newtype GroupId
  = GroupId Int

derive instance eqGroupId :: Eq GroupId
derive instance ordGroupId :: Ord GroupId

instance showGroupId :: Show GroupId where 
  show (GroupId gid) = "GroupId(" <> (show gid) <> ")"

derive instance genGroupId :: Generic GroupId _

instance encodeGroupId :: Encode GroupId where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

instance decodeGroupId :: Decode GroupId where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x
