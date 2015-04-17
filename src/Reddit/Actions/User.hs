module Reddit.Actions.User
  ( getUserInfo
  , aboutMe
  , getUserComments
  , getUserComments'
  , isUsernameAvailable
  , getBlockedUsers
  , getFriends
  , lookupUserFlair
  , setUserFlair ) where

import Reddit.Routes.Run
import Reddit.Types.Comment
import Reddit.Types.Empty
import Reddit.Types.Flair hiding (user)
import Reddit.Types.Error
import Reddit.Types.Options
import Reddit.Types.Reddit
import Reddit.Types.Subreddit
import Reddit.Types.User
import qualified Reddit.Routes.User as Route

import Control.Monad.IO.Class
import Data.Default
import Data.Text (Text)
import Network.API.Builder.Error
import qualified Data.Text as Text

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

lookupUserFlair :: MonadIO m => SubredditName -> Username -> RedditT m Flair
lookupUserFlair r u = do
  res <- runRoute $ Route.lookupUserFlair r u
  case res of
    FlairList (f:[]) _ _ -> return f
    _ -> failWith $ APIError InvalidResponseError

setUserFlair :: MonadIO m => SubredditName -> Username -> Text -> Text -> RedditT m ()
setUserFlair r u txt cls =
  if Text.length txt > 64
    then fail "Flair text too long!"
    else nothing $ runRoute $ Route.setUserFlair r u txt cls
