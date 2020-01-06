{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Graphql where

import Control.Monad.Base (MonadBase)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Trans (MonadIO, MonadTrans)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types
import Data.Morpheus.Types.Internal.AST (OperationType)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)

-- |
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
        , mutationResolver = Mutation {register = undefined}
        , subscriptionResolver = Undefined
        }


