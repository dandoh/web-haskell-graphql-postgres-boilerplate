module Graphql.Resolver.User where

import Authentication.JWT
import Authentication.Password
import Config
import Control.Monad.Base (MonadBase)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad.Trans (MonadIO, MonadTrans, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types
import Data.Morpheus.Types.Internal.AST (OperationType)
import Data.Pool (Pool, withResource)
import Data.Profunctor.Product.Default (Default)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Clock (getCurrentTime)
import Database.Model
import Database.PostgreSQL.Simple (Connection)
import Database.User
import GHC.Int (Int64)
import Graphql
import qualified Opaleye
import Opaleye (FromFields, Select)

-------------------------------------------------------------------------------
userResolver :: GraphQL o => UserData -> Object o User
userResolver UserData {userId, userEmail, userName} =
    return User {id = pure userId, email = pure userEmail, name = pure userName}

-------------------------------------------------------------------------------
loginResolver :: GraphQL o => LoginArgs -> Object o Session
loginResolver LoginArgs {email, password} = do
    res <- lift $ runSelect $ findUserByEmail email
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
    res :: [UserData] <- lift $ runSelect $ findUserByEmail email
    case res of
        _:_ -> failRes "This email is already taken"
        [] -> do
            ph <- liftIO $ hashPassword password
            lift $ runInsert $ insertUser (email, ph, name)
            loginResolver LoginArgs {email, password}
