module Reddit.API.Actions.Flair where

import Reddit.API.Routes.Flair
import Reddit.API.Routes.Run
import Reddit.API.Types.Flair
import Reddit.API.Types.Options
import Reddit.API.Types.Reddit
import Reddit.API.Types.Subreddit

import Control.Monad.IO.Class
import Data.Default
import Data.Text (Text)

getFlairList :: MonadIO m => SubredditName -> RedditT m FlairList
getFlairList = getFlairList' def

getFlairList' :: MonadIO m => Options Text -> SubredditName -> RedditT m FlairList
getFlairList' opts r = runRoute $ flairList opts r
