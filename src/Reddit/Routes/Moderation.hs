module Reddit.Routes.Moderation where

import Reddit.Types.Subreddit

import Data.Text (Text)
import Network.API.Builder.Routes

bansListing :: SubredditName -> Route
bansListing (R sub) =
  Route [ "r", sub, "about", "banned" ]
        [ ]
        "GET"
