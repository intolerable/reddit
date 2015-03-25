module Reddit.Actions.Comment
  ( getNewComments
  , getNewComments'
  , getMoreChildren
  , getCommentInfo
  , getCommentsInfo
  , editComment
  , deleteComment
  , removeComment ) where

import Reddit.Routes.Run
import Reddit.Types.Comment
import Reddit.Types.Empty
import Reddit.Types.Error
import Reddit.Types.Listing
import Reddit.Types.Options
import Reddit.Types.Post
import Reddit.Types.Reddit
import Reddit.Types.Subreddit
import qualified Reddit.Routes as Route

import Control.Monad.IO.Class
import Data.Default
import Data.Text (Text)
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

-- | Expand children comments that weren't fetched on initial load.
--   Equivalent to the web UI's "load more comments" button.
getMoreChildren :: MonadIO m
                => PostID -- ^ @PostID@ for the top-level
                -> [CommentID] -- ^ List of @CommentID@s to expand
                -> RedditT m [CommentReference]
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
    Listing _ _ [comment] -> return comment
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

-- | Edit a comment.
editComment :: MonadIO m
            => CommentID -- ^ Comment to edit
            -> Text -- ^ New comment text
            -> RedditT m Comment
editComment thing text = do
  POSTWrapped res <- runRoute $ Route.edit thing text
  return res

-- | Deletes one of your own comments. Note that this is different from
--   removing a comment as a moderator action.
deleteComment :: MonadIO m => CommentID -> RedditT m ()
deleteComment = nothing . runRoute . Route.delete

-- | Removes a comment (as a moderator action). Note that this is different
--   from deleting a comment.
removeComment :: MonadIO m => CommentID -> RedditT m ()
removeComment = nothing . runRoute . Route.removePost False
