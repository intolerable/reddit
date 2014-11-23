module Reddit.Routes.Flair where

import Reddit.Types.Options
import Reddit.Types.Subreddit
import Reddit.Types.User

import Network.API.Builder.Routes

flairList :: Options UserID -> SubredditName -> Route
flairList opts (R r) =
  Route [ "r", r, "api", "flairlist" ]
        [ "after" =. after opts
        , "before" =. before opts
        , "limit" =. limit opts ]
        "GET"
