module Error (Error, simpleError, ErrorResponse, toErrorResponse) where

import Data.Aeson (ToJSON (..), Value)
import GHC.Generics (Generic)

data Error = Error {message :: String, extensions :: Maybe Value}
  deriving (Show, Eq, Generic)

newtype ErrorResponse = ErrorResponse {errors :: [Error]}
  deriving (Show, Eq, Generic)

instance ToJSON Error

instance ToJSON ErrorResponse

-------------------------------------------------------------------------------
simpleError :: String -> Error
simpleError message = Error message Nothing

-------------------------------------------------------------------------------
toErrorResponse :: Error -> ErrorResponse
toErrorResponse error = ErrorResponse [error]
