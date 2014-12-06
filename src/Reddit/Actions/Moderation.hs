module Reddit.Actions.Moderation where

import Reddit.Routes.Moderation
import Reddit.Routes.Run
import Reddit.Types.Error
import Reddit.Types.Listing
import Reddit.Types.Moderation
import Reddit.Types.Options
import Reddit.Types.Reddit
import Reddit.Types.Subreddit
import Reddit.Types.User

import Network.API.Builder.Error

bans :: Options BanID -> SubredditName -> Reddit (Listing BanID Ban)
bans opts r = runRoute $ bansListing opts r

lookupBan :: Username -> SubredditName -> Reddit (Maybe Ban)
lookupBan u r = do
  Listing _ _ bs <- runRoute $ banLookup u r :: Reddit (Listing BanID Ban)
  case bs of
    b : [] -> return $ Just b
    [] -> return Nothing
    _-> failWith (APIError InvalidResponseError)
