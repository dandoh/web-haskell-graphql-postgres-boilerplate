
import Example
import System.Environment (lookupEnv)
import Config

main :: IO ()
main = do
    eitherConfig <- loadConfig
    case eitherConfig of
        Left errStr -> putStrLn errStr
        Right config -> do
            print config


