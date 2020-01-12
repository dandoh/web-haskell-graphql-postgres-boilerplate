module Database.Base where

import Data.Function ((&))
import Data.Profunctor.Product.Default (Default)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Data.Time (UTCTime)
import Opaleye
import Opaleye.Internal.Manipulation (Updater)

-------------------------------------------------------------------------------
type F a = Field a

-------------------------------------------------------------------------------
data EntityT a b c
  = Entity
      { record :: a,
        recordCreatedAt :: b,
        recordUpdatedAt :: c
      }

$(makeAdaptorAndInstance "pEntity" ''EntityT)

-------------------------------------------------------------------------------
type Entity a =
  EntityT
    a
    UTCTime
    UTCTime

type EntityWriteField a =
  EntityT
    a
    (Maybe (F SqlTimestamptz))
    (Maybe (F SqlTimestamptz))

type EntityField a =
  EntityT
    a
    (F SqlTimestamptz)
    (F SqlTimestamptz)

-------------------------------------------------------------------------------
withTimestampFields ::
  a ->
  EntityT
    a
    (TableFields (Maybe (F SqlTimestamptz)) (F SqlTimestamptz))
    (TableFields (Maybe (F SqlTimestamptz)) (F SqlTimestamptz))
withTimestampFields mapping =
  Entity
    { record = mapping,
      recordCreatedAt = tableField "created_at",
      recordUpdatedAt = tableField "updated_at"
    }

-------------------------------------------------------------------------------
withTimestamp :: [row] -> [EntityT row (Maybe timestamp) (Maybe timestamp)]
withTimestamp = map f
  where
    f r = Entity {record = r, recordCreatedAt = Nothing, recordUpdatedAt = Nothing}

-------------------------------------------------------------------------------
updateRecord ::
  Default Updater (EntityT record t t) fieldsW =>
  (record -> record) ->
  EntityT record t t ->
  fieldsW
updateRecord f = updateEasy (\r -> r {record = f (record r)})
