module Database.Common where

import Config
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import Data.Pool
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.URL

createConnectionsPool :: Config -> Init (Pool Connection)
createConnectionsPool config =
    case parseDatabaseUrl . T.unpack . databaseUrl $ config of
        Just connectionInfo ->
            liftIO $ createPool (connect connectionInfo) close 2 5 10
        _ -> throwError "Invalid database url"
