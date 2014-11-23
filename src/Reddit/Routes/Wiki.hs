module Reddit.Routes.Wiki where

import Reddit.Types.Subreddit

import Data.Text (Text)
import Network.API.Builder.Routes

wikiPage :: SubredditName -> Text -> Route
wikiPage (R sub) page =
  Route [ "r", sub, "wiki", page ]
        [ ]
        "GET"
