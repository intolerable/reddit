module Reddit.API.Actions.Comment
  ( getNewComments
  , getNewComments'
  , getMoreChildren
  , getCommentInfo
  , getCommentsInfo
  , deleteComment
  , removeComment ) where

import Reddit.API.Routes.Run
import Reddit.API.Types.Comment
import Reddit.API.Types.Empty
import Reddit.API.Types.Error
import Reddit.API.Types.Listing
import Reddit.API.Types.Options
import Reddit.API.Types.Post
import Reddit.API.Types.Reddit
import Reddit.API.Types.Subreddit
import qualified Reddit.API.Routes as Route

import Control.Monad.IO.Class
import Data.Default
import Network.API.Builder (APIError(..))

-- | Get a 'CommentListing' for the most recent comments on the site overall.
--   This maps to <http://reddit.com/r/$SUBREDDIT/comments>, or <http://reddit.com/comments>
--   if the subreddit is not specified.
--   Note that none of the comments returned will have any child comments.
getNewComments :: MonadIO m => Maybe SubredditName -> RedditT m CommentListing
getNewComments = getNewComments' def

-- | Get a 'CommentListing' for the most recent comments with the specified 'Options' and
--   'SubredditName'. Note that none of the comments returned will have any child comments.
--   If the 'Options' is 'def', then this function is identical to 'getNewComments'.
getNewComments' :: MonadIO m => Options CommentID -> Maybe SubredditName -> RedditT m CommentListing
getNewComments' opts r = runRoute $ Route.newComments opts r

getMoreChildren :: MonadIO m => PostID -> [CommentID] -> RedditT m [CommentReference]
getMoreChildren _ [] = return []
getMoreChildren p cs = do
  let (now, next) = splitAt 20 cs
  POSTWrapped rs <- runRoute $ Route.moreChildren p now
  more <- getMoreChildren p next
  return $ rs ++ more

-- | Given a 'CommentID', 'getCommentInfo' will return the full details for that comment.
getCommentInfo :: MonadIO m => CommentID -> RedditT m Comment
getCommentInfo c = do
  res <- getCommentsInfo [c]
  case res of
    Listing _ _ (comment:[]) -> return comment
    _ -> failWith $ APIError InvalidResponseError

-- | Given a list of 'CommentID's, 'getCommentsInfo' will return another list containing
--   the full details for all the comments.
getCommentsInfo :: MonadIO m => [CommentID] -> RedditT m CommentListing
getCommentsInfo cs =
  if null $ drop 100 cs
    then do
      res <- runRoute $ Route.commentsInfo cs
      case res of
        Listing _ _ comments | sameLength comments cs ->
          return res
        _ -> failWith $ APIError InvalidResponseError
    else failWith $ APIError TooManyRequests
  where
    sameLength (_:xs) (_:ys) = sameLength xs ys
    sameLength [] [] = True
    sameLength _ _ = False

-- | Deletes one of your own comments. Note that this is different from
--   removing a comment as a moderator action.
deleteComment :: MonadIO m => CommentID -> RedditT m ()
deleteComment = nothing . runRoute . Route.delete

-- | Removes a comment (as a moderator action). Note that this is different
--   from deleting a comment.
removeComment :: MonadIO m => CommentID -> RedditT m ()
removeComment = nothing . runRoute . Route.removePost False
