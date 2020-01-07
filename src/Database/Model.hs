module Database.Model where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Opaleye

-------------------------------------------------------------------------------
type F a = Field a

-------------------------------------------------------------------------------
data UserData' a b c d =
    UserData
        { userId :: a
        , userEmail :: b
        , userPasswordHash :: c
        , userName :: d
        }

type UserData = UserData' Int Text Text Text

-- `Maybe userId` because inserting doesn't require id
type UserWriteF
     = UserData' (Maybe (F SqlInt4)) (F SqlText) (F SqlText) (F SqlText)

type UserF = UserData' (F SqlInt4) (F SqlText) (F SqlText) (F SqlText)

$(makeAdaptorAndInstance "pUser" ''UserData')

userTable :: Table UserWriteF UserF
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
