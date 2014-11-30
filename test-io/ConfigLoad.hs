module ConfigLoad where

import Reddit
import Reddit.Types.Subreddit
import Reddit.Types.User
import System.Exit
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

newtype RunReddit = RunReddit
  { run :: forall a. Reddit a -> IO (Either (APIError RedditError) a) }

loadConfig :: IO (RunReddit, Username, SubredditName)
loadConfig = do
  file <- Text.readFile "test.cfg"
  case Text.lines file of
    user : pass : sub : [] ->
      return ( RunReddit $ runRedditWithRateLimiting user pass
             , Username user
             , R sub)
    _ -> do
      putStrLn "Invalid config"
      exitFailure
