module ConfigLoad where

import Control.Applicative
import Data.List
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

newtype Validation e a = Validation (Either e a)
  deriving (Show, Read, Eq, Ord, Functor)

instance Semigroup e => Applicative (Validation e) where
  pure = Validation . Right

  Validation a <*> Validation b =
    Validation $ case (a, b) of
      (Left ea, Left eb) -> Left (ea <> eb)
      (Left ea, Right _) -> Left ea
      (Right _, Left eb) -> Left eb
      (Right f, Right x) -> Right (f x)

optionsToConfig :: Opts -> Validation [String] Config
optionsToConfig Opts{..} =
  Config <$> get "optsUsername was missing" optsUsername
         <*> get "optsPassword was missing" optsPassword
         <*> get "optsSubreddit was missing" optsSubreddit
         <*> get "optsClientId was missing" optsClientId
         <*> get "optsClientSecret was missing" optsClientSecret
    where
      get :: String -> Maybe a -> Validation [String] a
      get _ (Just x) = pure x
      get msg Nothing = Validation (Left [msg])

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
    Validation (Left errs) ->
      error $
        intercalate "\n" $
          [ "Options were missing, got:"
          , "  " <> show (jsonOpts <> envOpts)
          , "Errors:"
          , intercalate "\n" $ map ("  " <>) errs
          ]
    Validation (Right (Config user pass sub ci cs)) -> do
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
