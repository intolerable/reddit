module Reddit.API.Routes.Flair where

import Reddit.API.Types.Subreddit
import qualified Reddit.API.Types.Flair as Flair

import APIBuilder.Routes
import Data.Text (Text)

flairList :: Maybe Text -> Maybe Text -> Maybe Int -> SubredditName -> Route
flairList b a l (R r) =
  Route [ "r", r, "api", "flairlist" ]
        [ "after" =. a
        , "before" =. b
        , "limit" =. l ]
        "GET"
