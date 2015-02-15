module Reddit.Actions.Search where

import Reddit.Routes.Run
import Reddit.Routes.Search
import Reddit.Types.Options
import Reddit.Types.Post
import Reddit.Types.Reddit
import Reddit.Types.Subreddit
import qualified Reddit.Types.SearchOptions as Search

import Data.Text (Text)

search :: Maybe SubredditName -> Options PostID -> Search.Order -> Text -> Reddit PostListing
search sub opts order query =
  runRoute $ searchRoute sub opts order query
