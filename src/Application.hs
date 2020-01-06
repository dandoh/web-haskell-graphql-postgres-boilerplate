module Application where

import Config
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Data.Pool
import qualified Data.Text as T
import Database.Common
import Database.PostgreSQL.Simple

appInit :: Init (Config, Pool Connection)
appInit = do
    config <- loadConfig
    connectionPool <- createConnectionsPool config
    return (config, connectionPool)
