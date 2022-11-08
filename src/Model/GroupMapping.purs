module PureTabs.Model.GroupMapping where 


import Browser.Tabs (WindowId)
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show (class Show, show)
import Effect.Aff (Aff)
import Effect.Class.Console (error)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Prelude (Unit, bind, flip, map, pure, ($), (/=), (<*), (<>), (==), (>>=), (>>>))
import PureTabs.Browser.Sessions (getWindowValue, setWindowValue)
import PureTabs.Model.Group (GroupId)


newtype GroupData 
  = GroupData { groupId :: GroupId
              , name :: String
              }

derive instance genGroupData :: Generic GroupData _
derive instance newtypeGroupData :: Newtype GroupData _

instance showGroupData :: Show GroupData where 
  show = genericShow

instance encodeGroupData :: Encode GroupData where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

instance decodeGroupData :: Decode GroupData where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

groupData :: GroupId -> String -> GroupData
groupData gid name = GroupData { groupId: gid, name: name }

newtype SavedGroups = SavedGroups (Array GroupData)

derive instance genSavedGroups :: Generic SavedGroups _
derive instance newtypeSavedGroups :: Newtype SavedGroups _

retrieveGroups :: WindowId -> Aff (Array GroupData)
retrieveGroups winId = do 
  (groups :: (Maybe SavedGroups)) <- getWindowValue winId "groups"
  case groups of
       Just (SavedGroups groups') -> pure groups'
       Nothing -> pure [] <* error ("couldn't get key `groups` for window " <> (show winId))

type GroupsUpdate = (Array GroupData) -> (Array GroupData)

updateGroupsMapping :: WindowId -> GroupsUpdate -> Aff Unit
updateGroupsMapping winId updateGroups = do
  groups <- retrieveGroups winId
  let updatedGroups = updateGroups groups
  setWindowValue winId "groups" updatedGroups


createGroup :: GroupId -> String -> GroupsUpdate
createGroup gid name = 
  A.filter (unwrap >>> _.groupId >>> (/=) gid)
  >>> (flip A.snoc) (groupData gid name)

renameGroup :: GroupId -> String -> GroupsUpdate
renameGroup gid newName = 
    map $ case _ of 
               GroupData { groupId: gid' } | gid == gid' -> groupData gid newName
               other -> other

moveGroup :: GroupId -> Int -> GroupsUpdate
moveGroup gid to arr =
    fromMaybe arr $ do
      from <- A.findIndex (unwrap >>> _.groupId >>> (==) gid) arr
      group <- arr A.!! from
      A.deleteAt from arr >>= A.insertAt to group

deleteGroup :: GroupId -> GroupsUpdate
deleteGroup gid = A.filter (unwrap >>>_.groupId >>> (/=) gid)
