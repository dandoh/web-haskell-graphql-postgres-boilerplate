{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphql where

import Authentication.JWT
import Authentication.Password
import Config
import Control.Monad.Base (MonadBase)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad.Trans (MonadIO, MonadTrans, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
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
import qualified Opaleye
import Opaleye (FromFields, Insert, Select)

--
-------------------------------------------------------------------------------
data Env =
    Env
        { dbPool :: Pool Connection
        , config :: Config
        , currentUserId :: Maybe Int
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
runSelect ::
       Default FromFields fields haskells => Select fields -> Web [haskells]
runSelect select = do
    db <- asks dbPool
    liftIO $
        withResource db $ \connection -> Opaleye.runSelect connection select

-- |
runInsert :: Insert haskells -> Web haskells
runInsert insert = do
    db <- asks dbPool
    liftIO $
        withResource db $ \connection -> Opaleye.runInsert_ connection insert

-- |
-------------------------------------------------------------------------------
importGQLDocument "schema.graphql"

-------------------------------------------------------------------------------
-- | Resolve single value
type Value (o :: OperationType) a = Resolver o () Web a

-- | Resolve object (which includes other fields that need their own resolvers)
type Object (o :: OperationType) a = Resolver o () Web (a (Resolver o () Web))

-- | Resolve (Maybe object)
type OptionalObject (o :: OperationType) a
     = Resolver o () Web (Maybe (a (Resolver o () Web)))

-- | Resolve [object]
type ArrayObject (o :: OperationType) a
     = Resolver o () Web [a (Resolver o () Web)]

type GraphQL o
     = ( MonadIO (Resolver o () Web)
       , WithOperation o
       , MonadTrans (Resolver o ()))
