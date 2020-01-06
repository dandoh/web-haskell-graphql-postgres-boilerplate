module Database.Model where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Opaleye

-------------------------------------------------------------------------------
data UserData' a b c d =
    UserData
        { userId :: a
        , userEmail :: b
        , userPasswordHash :: c
        , userName :: d
        }

type UserData = UserData' Int Text Text Text

type UserWrite
     = UserData' (Maybe (Field SqlInt4)) (Field SqlText) (Field SqlText) (Field SqlText)

type UserRead
 = UserData' (Field SqlInt4) (Field SqlText) (Field SqlText) (Field SqlText)

$(makeAdaptorAndInstance "pUser" ''UserData')

userTable :: Table UserWrite UserRead
userTable =
    table
        "users"
        (pUser
             UserData
                 { userId = tableField "id"
                 , userEmail = tableField "email"
                 , userPasswordHash = tableField "password_hash"
                 , userName = tableField "name"
                 })
-------------------------------------------------------------------------------
