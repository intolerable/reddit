module Reddit.Actions.Moderation where

import Reddit.Routes.Moderation
import Reddit.Routes.Run
import Reddit.Types.Listing
import Reddit.Types.Moderation
import Reddit.Types.Options
import Reddit.Types.Reddit
import Reddit.Types.Subreddit

bans :: Options BanID -> SubredditName -> Reddit (Listing BanID Ban)
bans opts r = runRoute $ bansListing opts r
