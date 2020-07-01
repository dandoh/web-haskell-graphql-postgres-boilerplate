{-# LANGUAGE RankNTypes #-}

module Graphql.Resolver.User where

import Authentication.JWT
import Authentication.Password
import Config
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad.Trans (MonadIO, MonadTrans, liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types
import Data.Morpheus.Types.Internal.AST (OperationType)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Clock (getCurrentTime)
import Database.Base
import qualified Database.Model as DB
import Database.PostgreSQL.Simple (Connection)
import Database.User
import GHC.Int (Int64)
import Graphql

-------------------------------------------------------------------------------
userResolver :: GraphQL o => DB.User -> Value o User
userResolver user =
  let DB.User {userId, userEmail, userName} = record user
   in return
        User
          { id = pure userId,
            email = pure userEmail,
            name = pure userName,
            createdAt = pure . T.pack . show $ recordCreatedAt user,
            updatedAt = pure . T.pack . show $ recordUpdatedAt user
          }

-------------------------------------------------------------------------------
loginResolver :: GraphQL o => LoginArgs -> Value o Session
loginResolver LoginArgs {email, password} = do
  res :: [DB.User] <- runSelect $ findUserByEmail email
  case res of
    [user] | validateHashedPassword (DB.userPasswordHash . record $ user) password -> do
      time <- liftIO getCurrentTime
      secret <- lift $ asks (jwtSecret . config)
      let jwt = makeJWT time secret (DB.userId . record $ user)
      return Session {token = pure jwt, user = userResolver user}
    _ -> fail "Wrong email or password"

-------------------------------------------------------------------------------
registerResolver :: RegisterArgs -> Value MUTATION Session
registerResolver RegisterArgs {email, password, name} = do
  res :: [DB.User] <- runSelect $ findUserByEmail email
  case res of
    _ : _ -> fail "This email is already taken"
    [] -> do
      ph <- liftIO $ hashPassword password
      runInsert $ insertUser (email, ph, name)
      loginResolver LoginArgs {email, password}

-------------------------------------------------------------------------------
myUserInfoResolver :: Value QUERY User
myUserInfoResolver = do
  myUserId <- requireAuthorized
  runSelectOne (findUserByID myUserId) "Invalid user" >>= userResolver

-------------------------------------------------------------------------------
changePasswordResolver :: ChangePasswordArgs -> Value MUTATION Bool
changePasswordResolver ChangePasswordArgs {oldPassword, newPassword} = do
  myUserId <- requireAuthorized
  userData :: DB.User <- runSelectOne (findUserByID myUserId) "Invalid user"
  if validateHashedPassword (DB.userPasswordHash . record $ userData) oldPassword
    then do
      ph <- liftIO $ hashPassword newPassword
      runUpdate $ updateUserPassword myUserId ph
      return True
    else fail "Wrong old password"
