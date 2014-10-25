module Reddit.API.Routes.Flair where

import Reddit.API.Types.Options
import Reddit.API.Types.Subreddit
import Reddit.API.Types.User

import Network.API.Builder.Routes

flairList :: Options UserID -> SubredditName -> Route
flairList opts (R r) =
  Route [ "r", r, "api", "flairlist" ]
        [ "after" =. after opts
        , "before" =. before opts
        , "limit" =. limit opts ]
        "GET"
