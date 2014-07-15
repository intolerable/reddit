module Reddit.API.Actions.Voting
  ( upvotePost
  , downvotePost
  , unvotePost
  , voteOnComment ) where

import Reddit.API.Types
import Reddit.API.Types.Empty
import Reddit.API.Types.Reddit
import qualified Reddit.API.Routes as Route
import Reddit.API.Routes.Run

import Control.Monad.IO.Class

-- Voting on posts

voteOnPost :: MonadIO m => Int -> PostID -> RedditT m ()
voteOnPost dir = nothing . runRoute . Route.vote dir

upvotePost :: MonadIO m => PostID -> RedditT m ()
upvotePost = voteOnPost 1

unvotePost :: MonadIO m => PostID -> RedditT m ()
unvotePost = voteOnPost 0

downvotePost :: MonadIO m => PostID -> RedditT m ()
downvotePost = voteOnPost (-1)

-- Voting on comments

voteOnComment :: MonadIO m => Int -> CommentID -> RedditT m ()
voteOnComment dir = nothing . runRoute . Route.vote dir
