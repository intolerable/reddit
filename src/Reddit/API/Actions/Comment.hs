module Reddit.API.Actions.Comment where

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

getCommentInfo :: MonadIO m => CommentID -> RedditT m Comment
getCommentInfo = getCommentInfo' def

getCommentInfo' :: MonadIO m => Options CommentID -> CommentID -> RedditT m Comment
getCommentInfo' opts c = do
  res <- getCommentsInfo' opts [c]
  case res of
    Listing _ _ (comment:[]) -> return comment
    _ -> failWith $ APIError InvalidResponseError

getCommentsInfo :: MonadIO m => [CommentID] -> RedditT m CommentListing
getCommentsInfo = getCommentsInfo' def

getCommentsInfo' :: MonadIO m => Options CommentID -> [CommentID] -> RedditT m CommentListing
getCommentsInfo' opts cs =
  if null $ drop 100 cs
    then do
      res <- runRoute $ Route.commentsInfo opts cs
      case res of
        Listing _ _ comments | sameLength comments cs ->
          return res
        _ -> failWith $ APIError InvalidResponseError
    else failWith $ APIError TooManyRequests
  where
    sameLength (_:xs) (_:ys) = sameLength xs ys
    sameLength [] [] = True
    sameLength _ _ = False
