{-# LANGUAGE OverloadedStrings #-}

module Authentication.JWT where

import Data.Aeson.Types (Value(Bool, Number))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Web.JWT

userIDKey :: Text
userIDKey = "USER_ID"

makeJWT :: Text -> Int -> Text
makeJWT secret userId =
    let cs =
            mempty -- mempty returns a default JWTClaimsSet
                { iss = stringOrURI "webhaskell"
                , unregisteredClaims =
                      ClaimsMap $
                      Map.fromList [(userIDKey, Number $ fromIntegral userId)]
                }
        key = hmacSecret "secret-key"
     in encodeSigned key mempty cs
