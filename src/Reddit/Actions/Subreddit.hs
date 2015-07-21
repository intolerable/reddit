-- | Contains actions for interacting with a Subreddit as a whole.
module Reddit.Actions.Subreddit
  ( getSubredditInfo
  , getSubredditSettings
  , setSubredditSettings ) where

import Reddit.Routes.Run
import Reddit.Types
import Reddit.Types.Empty
import Reddit.Types.Subreddit
import Reddit.Types.SubredditSettings
import qualified Reddit.Routes as Route

import Control.Monad.IO.Class

-- | Get the info for a specific subreddit. This info includes things like
--   sidebar contents, description and ID.
getSubredditInfo :: MonadIO m => SubredditName -> RedditT m Subreddit
getSubredditInfo = runRoute . Route.aboutSubreddit

-- | Get the settings for a subreddit that you moderate.
getSubredditSettings :: MonadIO m => SubredditName -> RedditT m SubredditSettings
getSubredditSettings = runRoute . Route.subredditSettings

-- | Modify the settings for a subreddit that you moderate.
setSubredditSettings :: MonadIO m => SubredditID -> SubredditSettings -> RedditT m ()
setSubredditSettings r s = nothing $ runRoute (Route.setSubredditSettings r s)
