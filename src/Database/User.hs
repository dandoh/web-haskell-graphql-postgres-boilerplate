module Database.User where

import Data.Text (Text)
import Database.Common
import Database.Model
import GHC.Int (Int64)
import Opaleye

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

