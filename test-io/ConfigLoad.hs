module ConfigLoad where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Reddit
import Reddit.Login
import System.Exit
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

newtype RunReddit = RunReddit
  { run :: forall a. Reddit a -> IO (Either (APIError RedditError) a) }

loadConfig :: IO (RunReddit, Username, SubredditName)
loadConfig = do
  file <- Text.readFile "test.cfg"
  case Text.lines file of
    [user, pass, sub] -> do
      manager <- newManager tlsManagerSettings
      res <- runRedditAnon $ login user pass
      case res of
        Left _ -> exitFailure
        Right details ->
          return ( RunReddit $ runRedditWith $ RedditOptions True (Just manager) (StoredDetails details) Nothing
                 , Username user
                 , R sub)
    _ -> do
      putStrLn "Invalid config"
      exitFailure
