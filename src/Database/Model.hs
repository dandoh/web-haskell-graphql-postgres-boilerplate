module Database.Model where

import Data.Function ((&))
import Data.Profunctor.Product.Default (Default)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Base
import Opaleye
import Opaleye.Internal.Manipulation (Updater)

-------------------------------------------------------------------------------
data UserT a b c d
  = User
      { userId :: a,
        userEmail :: b,
        userPasswordHash :: c,
        userName :: d
      }

$(makeAdaptorAndInstance "pUser" ''UserT)

type User =
  Entity
    ( UserT
        Int
        Text
        Text
        Text
    )

type UserWriteField =
  EntityWriteField
    ( UserT
        (Maybe (F SqlInt4)) -- use Maybe because we don't need to specify id when inserting
        (F SqlText)
        (F SqlText)
        (F SqlText)
    )

type UserField =
  EntityField
    ( UserT (F SqlInt4)
        (F SqlText)
        (F SqlText)
        (F SqlText)
    )

userTable :: Table UserWriteField UserField
userTable =
  table "users" . pEntity . withTimestampFields $
    pUser
      User
        { userId = tableField "id",
          userEmail = tableField "email",
          userPasswordHash = tableField "password_hash",
          userName = tableField "name"
        }
-------------------------------------------------------------------------------
