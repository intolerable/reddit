module Reddit.Actions.Wiki
  ( getWikiPage
  , editWikiPage ) where

import Reddit.Routes.Run
import Reddit.Types.Empty
import Reddit.Types.Reddit
import Reddit.Types.Subreddit
import Reddit.Types.Wiki
import qualified Reddit.Routes as Route

import Control.Monad.IO.Class
import Data.Text (Text)

-- | Get the specified wiki page on a subreddit.
getWikiPage :: MonadIO m => SubredditName -> Text -> RedditT m WikiPage
getWikiPage sub page = runRoute $ Route.wikiPage sub page

editWikiPage :: MonadIO m => SubredditName -> Text -> Text -> Text -> RedditT m ()
editWikiPage sub page content reason = nothing $ runRoute $ Route.editPage sub page content reason
