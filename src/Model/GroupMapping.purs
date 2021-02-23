module PureTabs.Model.GroupMapping where 


import Browser.Tabs (WindowId)
import Browser.Utils (unsafeLog)
import Control.Bind ((<#>))
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as M
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Semigroup ((<>))
import Data.Show (class Show)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, log)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Prelude (Unit, bind, flip, join, map, pure, ($), (*>), (/=), (<*), (<<<), (==), (>>=), (>>>))
import PureTabs.Browser.Storage (storageLocalGet, storageLocalSet)
import PureTabs.Model.Group (GroupId)


newtype SavedGroupMapping
  = SavedGroupMapping { windowId :: WindowId
                      , groupId :: GroupId
                      , name :: String
                      }

savedGroupMapping :: WindowId -> GroupId -> String -> SavedGroupMapping
savedGroupMapping winId gid name = SavedGroupMapping { windowId: winId, groupId: gid, name: name }

derive instance genGroupMapping :: Generic SavedGroupMapping _
derive instance newtypeGroupMapping :: Newtype SavedGroupMapping _

instance showGroupMapping :: Show SavedGroupMapping where 
  show = genericShow

instance encodeGroupMapping :: Encode SavedGroupMapping where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

instance decodeGroupMapping :: Decode SavedGroupMapping where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

newtype SavedGroups = SavedGroups (Array SavedGroupMapping)

derive instance genSavedGroups :: Generic SavedGroups _
derive instance newtypeSavedGroups :: Newtype SavedGroups _

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

type GroupMapping = M.Map WindowId (Array GroupData)

loadMap :: Array (SavedGroupMapping) -> GroupMapping
loadMap = M.fromFoldableWith ((<>)) 
  <<< map (unwrap >>> \r -> Tuple r.windowId (A.singleton (groupData r.groupId r.name)))

saveMap :: GroupMapping -> SavedGroups
saveMap = 
  M.toUnfoldable
  >>> map (\(Tuple winId groups) -> groups <#> \(GroupData g) -> savedGroupMapping winId g.groupId g.name)
  >>> join
  >>> SavedGroups

retrieveGroups :: Aff (Array SavedGroupMapping)
retrieveGroups = do 
  (groups :: (Maybe SavedGroups)) <- storageLocalGet "groups"
  case groups of
       Just (SavedGroups groups') -> pure groups'
       Nothing -> pure [] <* error "couldn't get key `groups` from local storage"

retrieveGroups' :: Aff GroupMapping
retrieveGroups' = retrieveGroups <#> loadMap

retrieveGroupsAt :: WindowId -> Aff (Array GroupData)
retrieveGroupsAt winId = retrieveGroups' <#> (fromMaybe [] <<< M.lookup winId)

type GroupsUpdate = GroupMapping -> GroupMapping

updateGroupsMapping :: GroupsUpdate -> Aff Unit
updateGroupsMapping updateGroups = do
  groups <- retrieveGroups'
  _ <- liftEffect $ (log "[bg] old groups:") *> (unsafeLog $ saveMap groups)
  let updatedGroups = updateGroups groups
  _ <- liftEffect $ (log "[bg] new groups:") *> (unsafeLog $ saveMap updatedGroups)
  storageLocalSet "groups" $ saveMap updatedGroups


updateMappingAt :: WindowId -> (Array GroupData -> Array GroupData) -> GroupsUpdate
updateMappingAt winId update = M.update (update >>> Just) winId

createGroup :: WindowId -> GroupId -> String -> GroupsUpdate
createGroup winId gid name = insertIfNotExist >>> createGroup'
  where
        insertIfNotExist mapping = if M.member winId mapping then mapping else M.insert winId [] mapping
        createGroup' = updateMappingAt winId $ 
                         A.filter (unwrap >>> _.groupId >>> (/=) gid)
                         >>> (flip A.snoc) (groupData gid name) 

renameGroup :: WindowId -> GroupId -> String -> GroupsUpdate
renameGroup winId gid newName = 
  updateMappingAt winId $ 
    map $ case _ of 
               GroupData { groupId: gid' } | gid == gid' -> groupData gid newName
               other -> other

moveGroup :: WindowId -> GroupId -> Int -> GroupsUpdate
moveGroup winId gid to =
  updateMappingAt winId $ \arr ->
    fromMaybe arr $ do
      from <- A.findIndex (unwrap >>> _.groupId >>> (==) gid) arr
      group <- arr A.!! from
      A.deleteAt from arr >>= A.insertAt to group

deleteGroup :: WindowId -> GroupId -> GroupsUpdate
deleteGroup winId gid = updateMappingAt winId $ A.filter (unwrap >>>_.groupId >>> (/=) gid)
