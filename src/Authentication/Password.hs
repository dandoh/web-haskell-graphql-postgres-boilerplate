module Authentication.Password where

import Crypto.BCrypt
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import qualified Data.Text as T

bs2t :: BS.ByteString -> T.Text
bs2t = T.pack . BS.unpack

t2bs :: T.Text -> BS.ByteString
t2bs = BS.pack . T.unpack

hashPassword :: T.Text -> IO T.Text
hashPassword =
  fmap (bs2t . fromJust)
    . hashPasswordUsingPolicy fastBcryptHashingPolicy
    . t2bs

validateHashedPassword :: T.Text -> T.Text -> Bool
validateHashedPassword hashedPassword originalPassword =
  validatePassword (t2bs hashedPassword) (t2bs originalPassword)
