module Database.Common where
import Config
import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple
import Data.Pool


createConnectionsPool :: Config -> IO (Pool Connection)
createConnectionsPool = undefined

