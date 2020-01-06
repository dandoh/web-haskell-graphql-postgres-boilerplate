{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Graphql where

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
import Database.PostgreSQL.Simple (Connection)
import Database.User
import qualified Opaleye
import Opaleye (FromFields, Select)

import Database.Model (UserData, UserField) -- |

--
-------------------------------------------------------------------------------
type Headers = [(Text, Text)]

data Env =
    Env
        { reqHeaders :: Headers
        , dbPool :: Pool Connection
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
        { queryResolver = Query {login = undefined}
        , mutationResolver = Mutation {register = registerResolver}
        , subscriptionResolver = Undefined
        }

-------------------------------------------------------------------------------
registerResolver :: RegisterArgs -> OptionalObject MUTATION Session
registerResolver RegisterArgs {email, password, name} = do
    db <- lift $ asks dbPool
    res <- lift $ runSelect @UserField @UserData $ findUserByEmail email
    case res of 
        _:_ -> failRes "This email is already taken"
        [] -> do 
            undefined
