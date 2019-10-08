module ConfigLoad where

import Control.Applicative
import Data.Text (Text)
import Data.Yaml
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Reddit
import Reddit.Login
import System.Directory
import System.Environment
import System.Exit
import Utils
import qualified Data.Text as Text

newtype RunReddit = RunReddit
  { run :: forall a. Reddit a -> IO (Either (APIError RedditError) a) }

data Opts =
  Opts { optsUsername :: Maybe Text
       , optsPassword :: Maybe Text
       , optsSubreddit :: Maybe Text
       , optsClientId :: Maybe Text
       , optsClientSecret :: Maybe Text
       }
  deriving (Show, Eq, Ord)

instance Semigroup Opts where
  Opts u1 p1 s1 ci1 cs1 <> Opts u2 p2 s2 ci2 cs2 =
    Opts (u2 <|> u1)
         (p2 <|> p1)
         (s2 <|> s1)
         (ci2 <|> ci1)
         (cs2 <|> cs1)

instance Monoid Opts where
  mappend = (<>)
  mempty = Opts mempty mempty mempty mempty mempty

instance FromJSON Opts where
  parseJSON = withObject "Opts" $ \ o ->
    Opts <$> o .:? "username"
         <*> o .:? "password"
         <*> o .:? "subreddit"
         <*> o .:? "client_id"
         <*> o .:? "client_secret"

getEnvOpts :: IO Opts
getEnvOpts =
  Opts <$> lookupEnv' "REDDIT_USERNAME"
       <*> lookupEnv' "REDDIT_PASSWORD"
       <*> lookupEnv' "REDDIT_SUBREDDIT"
       <*> lookupEnv' "REDDIT_CLIENT_ID"
       <*> lookupEnv' "REDDIT_CLIENT_SECRET"
    where
      lookupEnv' x = fmap (fmap Text.pack) (lookupEnv x)

data Config =
  Config { cfgUsername :: Text
         , cfgPassword :: Text
         , cfgSubreddit :: Text
         , cfgClientId :: Text
         , cfgClientSecret :: Text
         }
  deriving (Show, Eq, Ord)

optionsToConfig :: Opts -> Maybe Config
optionsToConfig Opts{..} =
  Config <$> optsUsername
         <*> optsPassword
         <*> optsSubreddit
         <*> optsClientId
         <*> optsClientSecret

loadConfig :: IO (RunReddit, Username, SubredditName)
loadConfig = do
  jsonOpts <- do
    let configFilePath = "test_config.yaml"
    doesFileExist configFilePath >>= \case
      False -> do
        putStrLn "Warning: missing config, skipping authorized tests"
        pure mempty
      True ->
        decodeFileEither "test_config.yaml" >>= \case
          Left err -> do
            print err
            exitFailure
          Right opts -> pure opts
  envOpts <- getEnvOpts

  case optionsToConfig (jsonOpts <> envOpts) of
    Nothing ->
      error $ "Options were missing, got " <> show (jsonOpts <> envOpts)
    Just (Config user pass sub ci cs) -> do
      manager <- newManager tlsManagerSettings
      res <- runAnon $ login user pass (ClientParams ci cs)
      case res of
        Left err -> do
          print err
          exitFailure
        Right details ->
          return ( RunReddit $ runRedditWith $ RedditOptions True (Just manager) (StoredDetails details) (Just "reddit-haskell test suite")
                 , Username user
                 , R sub)
