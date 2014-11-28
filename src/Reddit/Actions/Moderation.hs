module Reddit.Actions.Moderation where

import Reddit.Routes.Moderation
import Reddit.Routes.Run
import Reddit.Types.Listing
import Reddit.Types.Moderation
import Reddit.Types.Reddit
import Reddit.Types.Subreddit

import Data.Aeson

bans :: SubredditName -> Reddit (Listing BanID Ban)
bans r = runRoute $ bansListing r
