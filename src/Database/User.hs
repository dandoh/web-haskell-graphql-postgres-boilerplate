module Database.User where

import Control.Arrow (returnA)
import Data.Text (Text)
import Database.Model
import GHC.Int (Int64)
import Opaleye

-------------------------------------------------------------------------------
userSelect :: Select NewUserF
userSelect = selectTable newUserTable

-------------------------------------------------------------------------------
insertUser :: (Text, Text, Text) -> Insert Int64
insertUser (userEmail, userPasswordHash, userName) =
    Insert
        { iTable = newUserTable
        , iRows =
              withTimestamp
                  [ UserData
                        { userId = Nothing
                        , userEmail = toFields userEmail
                        , userPasswordHash = toFields userPasswordHash
                        , userName = toFields userName
                        }
                  ]
        , iReturning = rCount
        , iOnConflict = Nothing
        }

-------------------------------------------------------------------------------
findUserByEmail :: Text -> Select NewUserF
findUserByEmail email =
    proc () ->
  do user <- userSelect -< ()
     let userDetail = record user
     restrict -< userEmail userDetail .== toFields email
     returnA -< user

-------------------------------------------------------------------------------
findUserByID :: Int -> Select NewUserF
findUserByID id =
    proc () ->
  do user <- userSelect -< ()
     let userDetail = record user
     restrict -< userId userDetail .== toFields id
     returnA -< user

-------------------------------------------------------------------------------
updateUserPassword :: Int -> Text -> Update Int64
updateUserPassword id newPasswordHash =
    Update
        { uTable = newUserTable
        , uUpdateWith = updateRecord updatePasswordHash
        , uWhere = ((.==) (toFields id)) . userId . record
        , uReturning = rCount
        }
  where
    updatePasswordHash userData =
        userData {userPasswordHash = toFields newPasswordHash}
