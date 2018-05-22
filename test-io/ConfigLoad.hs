module ConfigLoad where

import Data.Text (Text)
import Data.Yaml
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Reddit
import Reddit.Login
import System.Exit
import Utils

newtype RunReddit = RunReddit
  { run :: forall a. Reddit a -> IO (Either (APIError RedditError) a) }

newtype TestConfig = TestConfig (Maybe Config)
  deriving (Show, Eq, Ord)

instance FromJSON TestConfig where
  parseJSON = withObject "TestConfig" $ \ o ->
    TestConfig <$> o .:? "config"

data Config =
  Config { tcUsername :: Text
         , tcPassword :: Text
         , tcSubreddit :: Text
         }
  deriving (Show, Eq, Ord)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \ o ->
    Config <$> o .: "username"
           <*> o .: "password"
           <*> o .: "subreddit"

loadConfig :: IO (RunReddit, Username, SubredditName)
loadConfig = do
  decodeFileEither "test_config.yaml" >>= \case
    Left err -> do
      print err
      exitFailure
    Right (TestConfig Nothing) -> do
      putStrLn "Warning: missing config section, skipping authorized tests"
      exitSuccess
    Right (TestConfig (Just (Config user pass sub))) -> do
      manager <- newManager tlsManagerSettings
      res <- runAnon $ login user pass
      case res of
        Left err -> do
          print err
          exitFailure
        Right details ->
          return ( RunReddit $ runRedditWith $ RedditOptions True (Just manager) (StoredDetails details) (Just "reddit-haskell test suite")
                 , Username user
                 , R sub)
