module Reddit.API.Actions.Comment where

import Reddit.API.Types.Comment
import Reddit.API.Types.Listing
import Reddit.API.Types.Post
import Reddit.API.Types.Reddit
import Reddit.API.Types.Subreddit
import Reddit.API.Routes.Run
import qualified Reddit.API.Routes as Route

import Control.Monad.IO.Class

getMoreChildren :: MonadIO m => PostID -> [CommentID] -> RedditT m [CommentReference]
getMoreChildren _ [] = return []
getMoreChildren p cs = do
  let (now, next) = splitAt 20 cs
  POSTWrapped rs <- runRoute $ Route.moreChildren p now
  more <- getMoreChildren p next
  return $ rs ++ more

getNewSubredditComments :: MonadIO m => SubredditName -> RedditT m [CommentReference]
getNewSubredditComments r = do
  Listing cs <- runRoute $ Route.newSubredditComments r
  return cs
