module Reddit.API.Actions.Voting
  ( upvotePost
  , downvotePost
  , unvotePost
  , upvoteComment
  , downvoteComment
  , unvoteComment ) where

import Reddit.API.Routes.Run
import Reddit.API.Types
import Reddit.API.Types.Empty
import Reddit.API.Types.Reddit
import qualified Reddit.API.Routes as Route

import Control.Monad.IO.Class

vote :: (MonadIO m, Thing a) => Int -> a -> RedditT m ()
vote dir = nothing . runRoute . Route.vote dir

-- | Upvote a post.
upvotePost :: MonadIO m => PostID -> RedditT m ()
upvotePost = vote 1

-- | Downvote a post.
downvotePost :: MonadIO m => PostID -> RedditT m ()
downvotePost = vote (-1)

-- | Remove a vote from a post.
unvotePost :: MonadIO m => PostID -> RedditT m ()
unvotePost = vote 0

-- | Upvote a comment.
upvoteComment :: MonadIO m => CommentID -> RedditT m ()
upvoteComment = vote 1

-- | Downvote a comment.
downvoteComment :: MonadIO m => CommentID -> RedditT m ()
downvoteComment = vote 0

-- | Remove a previously-cast vote from a comment.
unvoteComment :: MonadIO m => CommentID -> RedditT m ()
unvoteComment = vote (-1)
