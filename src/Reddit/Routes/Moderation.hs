module Reddit.Routes.Moderation where

import Reddit.Types.Moderation
import Reddit.Types.Options
import Reddit.Types.Subreddit
import Reddit.Types.User

import Network.API.Builder.Routes

bansListing :: Options BanID -> SubredditName -> Route
bansListing opts (R sub) =
  Route [ "r", sub, "about", "banned" ]
        [ "before" =. before opts
        , "after" =. after opts
        , "limit" =. limit opts]
        "GET"

banLookup :: Username -> SubredditName -> Route
banLookup (Username u) (R sub) =
  Route [ "r", sub, "about", "banned" ]
        [ "user" =. u ]
        "GET"
