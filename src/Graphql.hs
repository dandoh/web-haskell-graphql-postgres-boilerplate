{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Graphql where

import Authentication.JWT
import Authentication.Password
import Config
import Control.Monad.Base (MonadBase)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad.Trans (MonadIO, MonadTrans, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types
import Data.Morpheus.Types.Internal.AST (OperationType)
import Data.Pool (Pool, withResource)
import Data.Profunctor.Product.Default (Default)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Database.Model
import Database.PostgreSQL.Simple (Connection)
import Database.User
import GHC.Int (Int64)
import qualified Opaleye
import Opaleye (FromFields, Select)

--
-------------------------------------------------------------------------------
type Headers = [(Text, Text)]

data Env =
    Env
        { reqHeaders :: Headers
        , dbPool :: Pool Connection
        , config :: Config
        }

newtype Web a =
    Web
        { runWeb :: ReaderT Env IO a
        }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Env
             , MonadIO
             , MonadBase IO
             , MonadBaseControl IO
             )

-- | 
--
runSelect ::
       Default FromFields fields haskells => Select fields -> Web [haskells]
runSelect select = do
    db <- asks dbPool
    liftIO $
        withResource db $ \connection -> Opaleye.runSelect connection select

-- | 
--
runInsert :: Opaleye.Insert haskells -> Web haskells
runInsert insert = do
    db <- asks dbPool
    liftIO $
        withResource db $ \connection -> Opaleye.runInsert_ connection insert

-- |
--
-------------------------------------------------------------------------------
importGQLDocument "schema/schema.graphql"

-------------------------------------------------------------------------------
-- | Resolve single value
--
type Value (o :: OperationType) a = Resolver o () Web a

-- | Resolve object (which includes other fields that need their own resolvers)
--
type Object (o :: OperationType) a = Resolver o () Web (a (Resolver o () Web))

-- | Resolve (Maybe object)
--
type OptionalObject (o :: OperationType) a
     = Resolver o () Web (Maybe (a (Resolver o () Web)))

-- | Resolve [object]
--
type ArrayObject (o :: OperationType) a
     = Resolver o () Web [a (Resolver o () Web)]

type GraphQL o
     = ( MonadIO (Resolver o () Web)
       , WithOperation o
       , MonadTrans (Resolver o ()))

-------------------------------------------------------------------------------
rootResolver :: GQLRootResolver Web () Query Mutation Undefined
rootResolver =
    GQLRootResolver
        { queryResolver = Query {login = loginResolver}
        , mutationResolver = Mutation {register = registerResolver}
        , subscriptionResolver = Undefined
        }

-------------------------------------------------------------------------------
loginResolver :: GraphQL o => LoginArgs -> Object o Session
loginResolver LoginArgs {email, password} = do
    res <- lift $ runSelect @UserField @UserData $ findUserByEmail email
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
    res <- lift $ runSelect @UserField @UserData $ findUserByEmail email
    case res of
        _:_ -> failRes "This email is already taken"
        [] -> do
            ph <- liftIO $ hashPassword password
            lift $ runInsert $ insertUser (email, ph, name)
            loginResolver LoginArgs {email, password}

-------------------------------------------------------------------------------
userResolver :: GraphQL o => UserData -> Object o User
userResolver UserData {userId, userEmail, userName} =
    return User {id = pure userId, email = pure userEmail, name = pure userName}
