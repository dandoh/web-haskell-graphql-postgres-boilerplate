{-# LANGUAGE NamedFieldPuns #-}

module Config where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Text (Text)
import LoadEnv
import System.Environment (lookupEnv)

import GHC.Generics
import System.Envy
import Control.Monad.Except (ExceptT(..))

-- | 
--
-------------------------------------------------------------------------------
type Init a = ExceptT String IO a

data Config =
    Config
        { databaseUrl :: Text
        , jwtSecret :: Text
        }
    deriving (Generic, Show)

instance DefConfig Config where
    defConfig =
        Config
            { databaseUrl =
                  "postgres://Dandoh:dandoh@127.0.0.1:5432/webhaskell?sslmode=disable"
            , jwtSecret = "MY_SECRET_KEY"
            }

instance FromEnv Config

loadConfig :: Init Config
loadConfig = 
    ExceptT $ liftIO $ loadEnv >> decodeEnv


