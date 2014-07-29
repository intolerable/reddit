module Reddit.API.Actions.Flair where

import Reddit.API.Routes.Run
import Reddit.API.Routes.Flair
import Reddit.API.Types.Flair
import Reddit.API.Types.Reddit
import Reddit.API.Types.Subreddit

import Control.Monad.IO.Class
import Data.Text (Text)

getFlairList :: MonadIO m => Maybe Text -> Maybe Text -> Maybe Int -> SubredditName -> RedditT m FlairList
getFlairList b a l r = runRoute $ flairList b a l r
