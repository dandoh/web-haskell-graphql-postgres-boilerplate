module Database.Model where

import Data.Function ((&))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Data.Time (UTCTime)
import Opaleye
import Data.Profunctor.Product.Default (Default)
import Opaleye.Internal.Manipulation (Updater)

-------------------------------------------------------------------------------
type F a = Field a

-------------------------------------------------------------------------------
data EntityT a b c =
    EntityT
        { record :: a
        , createdAt :: b
        , updatedAt :: c
        }

$(makeAdaptorAndInstance "pEntity" ''EntityT)

-------------------------------------------------------------------------------
wrapEntityMapping :: a -> EntityT a (TableFields () ()) (TableFields () ())
wrapEntityMapping mapping =
    EntityT {record = mapping, createdAt = pure (), updatedAt = pure ()}

withTimestamps ::
       EntityT a (TableFields () ()) (TableFields () ())
    -> EntityT a (TableFields (Maybe (Field SqlTimestamptz)) (Field SqlTimestamptz)) (TableFields (Maybe (Field SqlTimestamptz)) (Field SqlTimestamptz))
withTimestamps mapping =
    mapping
        {createdAt = optional "created_at", updatedAt = optional "updated_at"}

-------------------------------------------------------------------------------
withTimestamp :: [row] -> [EntityT row (Maybe timestamp) (Maybe timestamp)]
withTimestamp = map f
  where
    f r = EntityT {record = r, createdAt = Nothing, updatedAt = Nothing}

updateRecord ::
       Default Updater (EntityT row t t) fieldsW
    => (row -> row)
    -> EntityT row t t
    -> fieldsW
updateRecord f = updateEasy (\r -> r {record = f (record r)})

-------------------------------------------------------------------------------
type Entity a = EntityT a UTCTime UTCTime

type EntityWriteF a
     = EntityT a (Maybe (F SqlTimestamptz)) (Maybe (F SqlTimestamptz))

type EntityReadF a = EntityT a (F SqlTimestamptz) (F SqlTimestamptz)

-------------------------------------------------------------------------------
data UserT a b c d =
    UserData
        { userId :: a
        , userEmail :: b
        , userPasswordHash :: c
        , userName :: d
        }

$(makeAdaptorAndInstance "pUser" ''UserT)

type UserData = UserT Int Text Text Text

type UserWrite = UserT (Maybe (F SqlInt4)) (F SqlText) (F SqlText) (F SqlText)

type UserRead = UserT (F SqlInt4) (F SqlText) (F SqlText) (F SqlText)



type NewUserData = Entity UserData

type NewUserWrite = EntityWriteF UserWrite

type NewUserF = EntityReadF UserRead

-------------------------------------------------------------------------------

newUserTable :: Table NewUserWrite NewUserF
newUserTable =
    table "users" $
    pEntity $
    pUser
        UserData
            { userId = tableField "id"
            , userEmail = tableField "email"
            , userPasswordHash = tableField "password_hash"
            , userName = tableField "name"
            } &
    wrapEntityMapping &
    withTimestamps

--userTable :: Table UserWrite UserRead
--userTable =
--    table
--        "users"
--        (pUser
--             UserData
--                 { userId = tableField "id"
--                 , userEmail = tableField "email"
--                 , userPasswordHash = tableField "password_hash"
--                 , userName = tableField "name"
--                 })
-------------------------------------------------------------------------------
