module Reddit.Actions.Search where

import Reddit.Routes.Search
import Reddit.Types.Options
import Reddit.Types.Post
import Reddit.Types.Reddit
import Reddit.Types.Subreddit
import qualified Reddit.Types.SearchOptions as Search

import Data.Text (Text)

search :: Monad m => Maybe SubredditName -> Options PostID -> Search.Order -> Text -> RedditT m PostListing
search sub opts order query =
  runRoute $ searchRoute sub opts order "plain" query

luceneSearch :: Monad m => Maybe SubredditName -> Options PostID -> Search.Order -> Text -> RedditT m PostListing
luceneSearch sub opts order query =
  runRoute $ searchRoute sub opts order "lucene" query

cloudSearch :: Monad m => Maybe SubredditName -> Options PostID -> Search.Order -> Text -> RedditT m PostListing
cloudSearch sub opts order query =
  runRoute $ searchRoute sub opts order "cloudsearch" query
