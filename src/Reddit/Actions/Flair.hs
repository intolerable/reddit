module Reddit.Actions.Flair
  ( getFlairList
  , getFlairList'
  , addLinkFlair
  , flairCSV ) where

import Reddit.Routes.Flair
import Reddit.Routes.Run
import Reddit.Types.Empty
import Reddit.Types.Flair
import Reddit.Types.Options
import Reddit.Types.Reddit
import Reddit.Types.Subreddit
import Reddit.Types.User

import Control.Monad.IO.Class
import Data.Aeson
import Data.Default
import Data.Text (Text)

-- | Get the flair list for a subreddit. Requires moderator privileges on
--   the subreddit.
getFlairList :: MonadIO m => SubredditName -> RedditT m FlairList
getFlairList = getFlairList' def

-- | Get the flair list for a subreddit (with 'Options'). Requires moderator
--   privileges on the subreddit.
getFlairList' :: MonadIO m => Options UserID -> SubredditName -> RedditT m FlairList
getFlairList' opts r = runRoute $ flairList opts r

addLinkFlair :: MonadIO m => SubredditName -> Text -> Text -> Bool -> RedditT m ()
addLinkFlair r c l e = do
  nothing $ runRoute $ addLinkFlairTemplate r c l e

flairCSV :: MonadIO m => SubredditName -> [(Username, Text, Text)] -> RedditT m Value
flairCSV r sets =
  runRoute $ flairCSVRoute r sets
