module Reddit.Actions.User
  ( getUserInfo
  , aboutMe
  , getUserComments
  , getUserComments'
  , isUsernameAvailable
  , getBlockedUsers
  , getFriends ) where

import Reddit.Routes.Run
import Reddit.Types.User
import Reddit.Types.Comment
import Reddit.Types.Options
import Reddit.Types.Reddit
import qualified Reddit.Routes.User as Route

import Control.Monad.IO.Class
import Data.Default

getUserInfo :: MonadIO m => Username -> RedditT m User
getUserInfo = runRoute . Route.aboutUser

getUserComments' :: MonadIO m => Options CommentID -> Username -> RedditT m CommentListing
getUserComments' opts user = runRoute $ Route.userComments opts user

getUserComments :: MonadIO m => Username -> RedditT m CommentListing
getUserComments = getUserComments' def

isUsernameAvailable :: MonadIO m => Username -> RedditT m Bool
isUsernameAvailable = runRoute . Route.usernameAvailable

aboutMe :: MonadIO m => RedditT m User
aboutMe = runRoute Route.aboutMe

getBlockedUsers :: MonadIO m => RedditT m [Relationship]
getBlockedUsers = do
  UserList rs <- runRoute Route.blocked
  return rs

getFriends :: MonadIO m => RedditT m [Relationship]
getFriends = do
  UserList rs <- runRoute Route.friends
  return rs
