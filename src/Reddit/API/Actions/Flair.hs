module Reddit.API.Actions.Flair
  ( getFlairList
  , getFlairList' ) where

import Reddit.API.Routes.Flair
import Reddit.API.Routes.Run
import Reddit.API.Types.Flair
import Reddit.API.Types.Options
import Reddit.API.Types.Reddit
import Reddit.API.Types.Subreddit
import Reddit.API.Types.User

import Control.Monad.IO.Class
import Data.Default

-- | Get the flair list for a subreddit. Requires moderator privileges on
--   the subreddit.
getFlairList :: MonadIO m => SubredditName -> RedditT m FlairList
getFlairList = getFlairList' def

-- | Get the flair list for a subreddit (with 'Options'). Requires moderator
--   privileges on the subreddit.
getFlairList' :: MonadIO m => Options UserID -> SubredditName -> RedditT m FlairList
getFlairList' opts r = runRoute $ flairList opts r
