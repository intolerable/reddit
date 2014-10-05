module Reddit.API.Actions.Comment where

import Reddit.API.Routes.Run
import Reddit.API.Types.Comment
import Reddit.API.Types.Empty
import Reddit.API.Types.Options
import Reddit.API.Types.Post
import Reddit.API.Types.Reddit
import Reddit.API.Types.Subreddit
import qualified Reddit.API.Routes as Route

import Control.Monad.IO.Class
import Data.Default

getMoreChildren :: MonadIO m => PostID -> [CommentID] -> RedditT m [CommentReference]
getMoreChildren _ [] = return []
getMoreChildren p cs = do
  let (now, next) = splitAt 20 cs
  POSTWrapped rs <- runRoute $ Route.moreChildren p now
  more <- getMoreChildren p next
  return $ rs ++ more

-- | Note that none of the comments returned will have any child comments.
getNewComments' :: MonadIO m => Options CommentID -> Maybe SubredditName -> RedditT m CommentListing
getNewComments' opts r = runRoute $ Route.newComments opts r

getNewComments :: MonadIO m => Maybe SubredditName -> RedditT m CommentListing
getNewComments = getNewComments' def

removeComment :: MonadIO m => CommentID -> RedditT m ()
removeComment = nothing . runRoute . Route.removePost False
