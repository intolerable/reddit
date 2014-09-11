module Reddit.API.Routes.Flair where

import Reddit.API.Types.Subreddit

import Data.Text (Text)
import Network.API.Builder.Routes

flairList :: Maybe Text -> Maybe Text -> Maybe Int -> SubredditName -> Route
flairList b a l (R r) =
  Route [ "r", r, "api", "flairlist" ]
        [ "after" =. a
        , "before" =. b
        , "limit" =. l ]
        "GET"
