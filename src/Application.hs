module Application where

import Authentication.JWT
import Config
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (find)
import Data.Morpheus (interpreter)
import Data.Pool
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Clock (getCurrentTime)
import Database.PostgreSQL.Simple
import Graphql
import Graphql.Resolver.Root
import Web.Scotty

-------------------------------------------------------------------------------
api :: B.ByteString -> Web B.ByteString
api = interpreter rootResolver

-------------------------------------------------------------------------------
app :: IO ()
app = do
    init <- runExceptT initialize
    case init of
        Left err -> putStrLn err
        Right res -> webServer res

-------------------------------------------------------------------------------
webServer :: (Config, Pool Connection) -> IO ()
webServer (config, connectionPool) =
    scotty 8080 $
    post "/api" $ do
        reqBody <- body
        reqHeaders <- headers
        currentTime <- liftIO getCurrentTime
        let currentUserId =
                case find ((== "Authorization") . fst) reqHeaders of
                    Just (_, token) ->
                        verifyJWT
                            currentTime
                            (jwtSecret config)
                            (T.pack . LT.unpack $ token)
                    _ -> Nothing
        let env = Env connectionPool config currentUserId
        raw =<< (liftIO . flip runReaderT env . runWeb $ api reqBody)
