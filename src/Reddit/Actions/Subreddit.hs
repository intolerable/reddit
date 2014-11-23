module Reddit.Actions.Subreddit
  ( getSubredditInfo
  , getSubredditSettings
  , setSubredditSettings ) where

import Reddit.Routes.Run
import Reddit.Types
import Reddit.Types.Empty
import Reddit.Types.Reddit
import Reddit.Types.Subreddit
import qualified Reddit.Routes as Route

import Control.Monad.IO.Class

getSubredditInfo :: MonadIO m => SubredditName -> RedditT m Subreddit
getSubredditInfo = runRoute . Route.aboutSubreddit

getSubredditSettings :: MonadIO m => SubredditName -> RedditT m SubredditSettings
getSubredditSettings = runRoute . Route.subredditSettings

setSubredditSettings :: MonadIO m => SubredditID -> SubredditSettings -> RedditT m ()
setSubredditSettings r s = nothing $ runRoute (Route.setSubredditSettings r s)
