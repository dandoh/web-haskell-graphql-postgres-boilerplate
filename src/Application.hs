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
import Network.HTTP.Types.Status (mkStatus, status200, status404)
import Network.Wai.Middleware.Cors
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
  scotty 8080 $ do
    let myCors = cors (const $ Just (simpleCorsResourcePolicy {corsRequestHeaders = ["Content-Type"]}))
    middleware myCors
    post "/api" $ do
      reqBody <- body
      reqHeaders <- headers
      currentTime <- liftIO getCurrentTime
      let currentUserId = case find ((== "Authorization") . fst) reqHeaders of
            Just (_, token) -> verifyJWT currentTime (jwtSecret config) (T.pack . LT.unpack $ token)
            _ -> Nothing
      let env = Env connectionPool config currentUserId
      response <- liftIO . flip runReaderT env . runExceptT . runWeb $ api reqBody
      case response of
        Left (Error errorStatus errorBody) -> do
          status (mkStatus errorStatus "")
          raw errorBody
        Right jsonResponse -> do
          setHeader "Content-Type" "application/json; charset=utf-8"
          status status200
          raw jsonResponse
    get "/graphiql" $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      file "graphiql.html"
    notFound $ do
      status status404
      text "Not found"
