module Database.User where

import Control.Arrow (returnA)
import Data.Text (Text)
import Database.Model
import GHC.Int (Int64)
import Opaleye

-------------------------------------------------------------------------------
userSelect :: Select UserF
userSelect = selectTable userTable

-------------------------------------------------------------------------------
insertUser :: (Text, Text, Text) -> Insert Int64
insertUser (userEmail, userPasswordHash, userName) =
    Insert
        { iTable = userTable
        , iRows =
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
findUserByEmail :: Text -> Select UserF
findUserByEmail email =
    proc () ->
  do user <- userSelect -< ()
     restrict -< userEmail user .== toFields email
     returnA -< user

-------------------------------------------------------------------------------
findUserByID :: Int -> Select UserF
findUserByID id =
    proc () ->
  do user <- userSelect -< ()
     restrict -< userId user .== toFields id
     returnA -< user

-------------------------------------------------------------------------------
updateUserPassword :: Int -> Text -> Update Int64
updateUserPassword id newPasswordHash =
    Update
        { uTable = userTable
        , uUpdateWith =
              updateEasy
                  (\userData ->
                       userData {userPasswordHash = toFields newPasswordHash})
        , uWhere = ((.==) (toFields id)) . userId
        , uReturning = rCount
        }
