module Reddit.API.Actions.Subreddit
  ( getSubredditInfo
  , getSubredditSettings
  , setSubredditSettings ) where

import Reddit.API.Routes.Run
import Reddit.API.Types
import Reddit.API.Types.Empty
import Reddit.API.Types.Reddit
import Reddit.API.Types.Subreddit
import qualified Reddit.API.Routes as Route

import Control.Monad.IO.Class

getSubredditInfo :: MonadIO m => SubredditName -> RedditT m Subreddit
getSubredditInfo = runRoute . Route.aboutSubreddit

getSubredditSettings :: MonadIO m => SubredditName -> RedditT m SubredditSettings
getSubredditSettings = runRoute . Route.subredditSettings

setSubredditSettings :: MonadIO m => SubredditName -> SubredditSettings -> RedditT m ()
setSubredditSettings r s = nothing $ runRoute (Route.setSubredditSettings r s)
