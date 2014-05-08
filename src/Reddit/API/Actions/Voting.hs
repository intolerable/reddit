module Reddit.API.Actions.Voting
  ( upvotePost
  , downvotePost
  , unvotePost
  , voteOnComment ) where

import Reddit.API.Types
import Reddit.API.Types.Empty
import Reddit.API.Types.Reddit
import qualified Reddit.API.Routes as Route

import APIBuilder

-- Voting on posts

voteOnPost :: Int -> PostID -> Reddit ()
voteOnPost dir = nothing . Reddit . runRoute . Route.vote dir

upvotePost :: PostID -> Reddit ()
upvotePost = voteOnPost 1

unvotePost :: PostID -> Reddit ()
unvotePost = voteOnPost 0

downvotePost :: PostID -> Reddit ()
downvotePost = voteOnPost (-1)

-- Voting on comments

voteOnComment :: Int -> CommentID -> Reddit ()
voteOnComment dir = nothing . Reddit . runRoute . Route.vote dir
