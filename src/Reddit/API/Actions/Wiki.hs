module Reddit.API.Actions.Wiki
  ( getWikiPage ) where

import Reddit.API.Routes.Run
import Reddit.API.Types.Reddit
import Reddit.API.Types.Subreddit
import Reddit.API.Types.Wiki
import qualified Reddit.API.Routes as Route

import Control.Monad.IO.Class
import Data.Text (Text)

getWikiPage :: MonadIO m => SubredditName -> Text -> RedditT m WikiPage
getWikiPage sub page = runRoute $ Route.wikiPage sub page
