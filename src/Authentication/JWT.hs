{-# LANGUAGE OverloadedStrings #-}

module Authentication.JWT where

import Control.Monad (guard)
import Data.Aeson.Types (Value (Bool, Number))
import qualified Data.Map as Map
import Data.Scientific (base10Exponent, coefficient)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX
import Web.JWT
import Prelude hiding (exp)

-------------------------------------------------------------------------------
type SecretKey = Text

type Token = Text

userIDKey :: Text
userIDKey = "USERID"

-- |
-------------------------------------------------------------------------------
makeJWT :: UTCTime -> SecretKey -> Int -> Token
makeJWT currentTime secret userId =
  let cs =
        mempty -- mempty returns a default JWTClaimsSet
          { iss = stringOrURI "webhaskell",
            unregisteredClaims =
              ClaimsMap $
                Map.fromList [(userIDKey, Number $ fromIntegral userId)],
            exp =
              numericDate $
                utcTimeToPOSIXSeconds currentTime + 30 * posixDayLength
          }
      signer = hmacSecret secret
   in encodeSigned signer mempty cs

-- |
-------------------------------------------------------------------------------
verifyJWT :: UTCTime -> SecretKey -> Token -> Maybe Int
verifyJWT currentTime secret token = do
  let signer = hmacSecret secret
  unverifiedJWT <- decode token
  verifiedJWT <- verify signer unverifiedJWT
  expTime <- exp . claims $ verifiedJWT
  now <- numericDate $ utcTimeToPOSIXSeconds currentTime
  guard (now < expTime)
  let kv = unClaimsMap . unregisteredClaims . claims $ verifiedJWT
  userIDVal <- Map.lookup userIDKey kv
  case userIDVal of
    Number userID -> return . fromIntegral $ coefficient userID
    _ -> Nothing
