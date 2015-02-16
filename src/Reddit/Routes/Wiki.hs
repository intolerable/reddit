module Reddit.Routes.Wiki where

import Reddit.Types.Subreddit

import Data.Text (Text)
import Network.API.Builder.Routes

wikiPage :: SubredditName -> Text -> Route
wikiPage (R sub) page =
  Route [ "r", sub, "wiki", page ]
        [ ]
        "GET"

editPage :: SubredditName -> Text -> Text -> Text -> Route
editPage (R sub) page content reason =
  Route [ "r", sub, "api", "wiki", "edit" ]
        [ "page" =. page
        , "content" =. content
        --, "previous" =. previous
        , "reason" =. reason ]
        "POST"
