module Reddit.API.Actions.Wiki where

import Reddit.API.Routes.Run
import Reddit.API.Types.Reddit
import Reddit.API.Types.Subreddit
import Reddit.API.Types.Wiki
import qualified Reddit.API.Routes as Route

import Data.Text (Text)

getWikiPage :: SubredditName -> Text -> Reddit WikiPage
getWikiPage sub page = runRoute $ Route.wikiPage sub page
