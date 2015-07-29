-- | Contains subreddit wiki-related actions.
module Reddit.Actions.Wiki
  ( getWikiPage
  , editWikiPage ) where

import Reddit.Types.Empty
import Reddit.Types.Reddit
import Reddit.Types.Subreddit
import Reddit.Types.Wiki
import qualified Reddit.Routes as Route

import Data.Text (Text)

-- | Get the specified wiki page on a particular subreddit. Requires
--   permission to view the specified wiki page.
getWikiPage :: Monad m => SubredditName -> Text -> RedditT m WikiPage
getWikiPage sub page = runRoute $ Route.wikiPage sub page

-- | Edit the specified wiki page on a particular subreddit. Requires
--   permission to edit the specified wiki page.
editWikiPage :: Monad m
             => SubredditName -- ^ Subreddit whose wiki to modify
             -> Text -- ^ The name of the page you're editing
             -> Text -- ^ The new markdown content of the page you're editing
             -> Text -- ^ The reason for the edit
             -> RedditT m ()
editWikiPage sub page content reason = nothing $ runRoute $ Route.editPage sub page content reason
