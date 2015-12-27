module Reddit.Actions.Moderation where

import Reddit.Routes.Moderation
import Reddit.Types.Error
import Reddit.Types.Listing
import Reddit.Types.Message
import Reddit.Types.Moderation
import Reddit.Types.Options
import Reddit.Types.Reddit
import Reddit.Types.Subreddit
import Reddit.Types.User

import Network.API.Builder.Error

-- | Get a list of existing bans on a subreddit.
--   User must be a moderator of the subreddit.
bans :: Monad m => Options BanID -> SubredditName -> RedditT m (Listing BanID Ban)
bans opts r = runRoute $ bansListing opts r

-- | Check to see if a user is banned from a subreddit. Logged-in user must
--   be a moderator of the subreddit
lookupBan :: Monad m => Username -> SubredditName -> RedditT m (Maybe Ban)
lookupBan u r = do
  res <- runRoute $ banLookup u r
  case res :: Listing BanID Ban of
    Listing _ _ bs ->
      case bs of
        [b] -> return $ Just b
        [] -> return Nothing
        _-> failWith (APIError InvalidResponseError)

getModmail :: Monad m => RedditT m (Listing MessageID Message)
getModmail = runRoute $ modmail $ Options Nothing Nothing

getModmail' :: Monad m => Options MessageID -> RedditT m (Listing MessageID Message)
getModmail' = runRoute . modmail
