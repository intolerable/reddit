module Reddit.API.Routes.Wiki where

import Reddit.API.Types.Subreddit

import APIBuilder.Routes
import Data.Text (Text)

wikiPage :: SubredditName -> Text -> Route
wikiPage (R sub) page =
  Route [ "r", sub, "wiki", page ]
        [ ]
        "GET"
