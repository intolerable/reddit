-- | Contains actions for handling flair on a subreddit-wise basis.
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

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Default.Class
import Data.Text (Text)

-- | Get the flair list for a subreddit. Requires moderator privileges on
--   the subreddit.
getFlairList :: MonadIO m => SubredditName -> RedditT m FlairListing
getFlairList = getFlairList' def

-- | Get the flair list for a subreddit (with 'Options'). Requires moderator
--   privileges on the subreddit.
getFlairList' :: MonadIO m => Options UserID -> SubredditName -> RedditT m FlairListing
getFlairList' opts r = liftM flistToListing $ runRoute (flairList opts r)

-- | Add link flair to the subreddit-wide template for a subreddit that you moderate.
--   Requires moderator privileges on the subreddit.
addLinkFlair :: MonadIO m
             => SubredditName -- ^ The subreddit whose template you want to modify
             -> Text -- ^ The intended CSS class of the new link flair
             -> Text -- ^ The intended text label of the new link flair
             -> Bool -- ^ Whether the flair should be editable by users
             -> RedditT m ()
addLinkFlair r c l e =
  nothing $ runRoute $ addLinkFlairTemplate r c l e

flairCSV :: MonadIO m => SubredditName -> [(Username, Text, Text)] -> RedditT m Value
flairCSV r sets =
  runRoute $ flairCSVRoute r sets
