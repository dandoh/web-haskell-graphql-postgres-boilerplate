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
import Database.Model
import Database.PostgreSQL.Simple (Connection)
import Database.User
import GHC.Int (Int64)
import Graphql

-------------------------------------------------------------------------------
userResolver :: GraphQL o => UserData -> Object o User
userResolver UserData {userId, userEmail, userName} =
    return User {id = pure userId, email = pure userEmail, name = pure userName}

-------------------------------------------------------------------------------
loginResolver :: GraphQL o => LoginArgs -> Object o Session
loginResolver LoginArgs {email, password} = do
    res <- runSelect $ findUserByEmail email
    case res of
        [userData]
            | validateHashedPassword (userPasswordHash userData) password -> do
                time <- liftIO getCurrentTime
                secret <- lift $ asks (jwtSecret . config)
                let jwt = makeJWT time secret (userId userData)
                return Session {token = pure jwt, user = userResolver userData}
        _ -> failRes "Wrong email or password"

-------------------------------------------------------------------------------
registerResolver :: RegisterArgs -> Object MUTATION Session
registerResolver RegisterArgs {email, password, name} = do
    res :: [UserData] <- runSelect $ findUserByEmail email
    case res of
        _:_ -> failRes "This email is already taken"
        [] -> do
            ph <- liftIO $ hashPassword password
            runInsert $ insertUser (email, ph, name)
            loginResolver LoginArgs {email, password}

-------------------------------------------------------------------------------
myUserInfoResolver :: Object QUERY User
myUserInfoResolver = do
    myUserId <- requireAuthorized
    runSelectOne (findUserByID myUserId) "Invalid user" >>= userResolver

-------------------------------------------------------------------------------
changePasswordResolver :: ChangePasswordArgs -> Value MUTATION Bool
changePasswordResolver ChangePasswordArgs {oldPassword, newPassword} = do
    myUserId <- requireAuthorized
    userData :: UserData <- runSelectOne (findUserByID myUserId) "Invalid user"
    if validateHashedPassword (userPasswordHash userData) oldPassword
        then do
            ph <- liftIO $ hashPassword newPassword
            runUpdate $ updateUserPassword myUserId ph
            return True
        else failRes "Wrong old password"
