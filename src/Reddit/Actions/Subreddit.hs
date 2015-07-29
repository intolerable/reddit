-- | Contains actions for interacting with a Subreddit as a whole.
module Reddit.Actions.Subreddit
  ( getSubredditInfo
  , getSubredditSettings
  , setSubredditSettings ) where

import Reddit.Types
import Reddit.Types.Empty
import Reddit.Types.Reddit
import Reddit.Types.Subreddit
import Reddit.Types.SubredditSettings
import qualified Reddit.Routes as Route

-- | Get the info for a specific subreddit. This info includes things like
--   sidebar contents, description and ID.
getSubredditInfo :: Monad m => SubredditName -> RedditT m Subreddit
getSubredditInfo = runRoute . Route.aboutSubreddit

-- | Get the settings for a subreddit that you moderate.
getSubredditSettings :: Monad m => SubredditName -> RedditT m SubredditSettings
getSubredditSettings = runRoute . Route.subredditSettings

-- | Modify the settings for a subreddit that you moderate.
setSubredditSettings :: Monad m => SubredditID -> SubredditSettings -> RedditT m ()
setSubredditSettings r s = nothing $ runRoute (Route.setSubredditSettings r s)
