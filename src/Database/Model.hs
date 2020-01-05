module Database.Model where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Opaleye

-------------------------------------------------------------------------------

data User' a b c =
    User
        { userId :: a
        , userEmail :: b
        , userPasswordHash :: c
        }

type User = User' Int String String

type UserField = User' (Field SqlInt4) (Field SqlText) (Field SqlText)

$(makeAdaptorAndInstance "pUser" ''User')

userTable :: Table UserField UserField
userTable =
    table
        "users"
        (pUser
             User
                 { userId = tableField "id"
                 , userEmail = tableField "email"
                 , userPasswordHash = tableField "password_hash"
                 })

-------------------------------------------------------------------------------
    

