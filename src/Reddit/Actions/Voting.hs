module Reddit.Actions.Voting
  ( upvotePost
  , downvotePost
  , unvotePost
  , upvoteComment
  , downvoteComment
  , unvoteComment ) where

import Reddit.Routes.Run
import Reddit.Types
import Reddit.Types.Empty
import Reddit.Types.Reddit
import qualified Reddit.Routes as Route

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
downvoteComment = vote (-1)

-- | Remove a previously-cast vote from a comment.
unvoteComment :: MonadIO m => CommentID -> RedditT m ()
unvoteComment = vote 0
