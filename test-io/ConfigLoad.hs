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

data TestConfig =
  TestConfig { tcUsername :: Text
             , tcPassword :: Text
             , tcSubreddit :: Text
             }
  deriving (Show, Eq, Ord)

instance FromJSON TestConfig where
  parseJSON = withObject "TestConfig" $ \ o ->
    TestConfig <$> o .: "username"
               <*> o .: "password"
               <*> o .: "subreddit"

loadConfig :: IO (RunReddit, Username, SubredditName)
loadConfig = do
  decodeFileEither "test_config.yaml" >>= \case
    Left err -> do
      print err
      exitFailure
    Right (TestConfig user pass sub) -> do
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
