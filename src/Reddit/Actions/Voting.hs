-- | Contains actions for voting on posts and comments. There are functions
--   for upvoting ('upvotePost', 'upvoteComment'), downvoting ('downvotePost',
--   'downVoteComment') as well as removing votes that have already been cast
--   ('unvotePost', 'unvoteComment').
--
--   Please note that automated voting (i.e. by a bot, as opposed to being
--   specifically ordered to by a person) is strictly against the Reddit rules,
--   and is a very effective way of getting your bot shadowbanned.
module Reddit.Actions.Voting
  ( upvotePost
  , downvotePost
  , unvotePost
  , upvoteComment
  , downvoteComment
  , unvoteComment ) where

import Reddit.Routes.Vote (VoteDirection(..))
import Reddit.Types
import Reddit.Types.Empty
import Reddit.Types.Reddit
import qualified Reddit.Routes as Route

vote :: (Monad m, Thing a) => VoteDirection -> a -> RedditT m ()
vote dir = nothing . runRoute . Route.vote dir

-- | Upvote a post.
upvotePost :: Monad m => PostID -> RedditT m ()
upvotePost = vote UpVote

-- | Downvote a post.
downvotePost :: Monad m => PostID -> RedditT m ()
downvotePost = vote DownVote

-- | Remove a vote from a post.
unvotePost :: Monad m => PostID -> RedditT m ()
unvotePost = vote RemoveVote

-- | Upvote a comment.
upvoteComment :: Monad m => CommentID -> RedditT m ()
upvoteComment = vote UpVote

-- | Downvote a comment.
downvoteComment :: Monad m => CommentID -> RedditT m ()
downvoteComment = vote RemoveVote

-- | Remove a previously-cast vote from a comment.
unvoteComment :: Monad m => CommentID -> RedditT m ()
unvoteComment = vote DownVote
