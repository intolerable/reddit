-- | Contains user-related actions, like finding friends or retrieving a
--   user's comments or information.
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
import Reddit.Types.Listing
import Reddit.Types.Options
import Reddit.Types.Reddit
import Reddit.Types.Subreddit
import Reddit.Types.User
import qualified Reddit.Routes.User as Route

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Text (Text)
import Network.API.Builder.Error
import qualified Data.Text as Text

-- | Get the information Reddit exposes on user behind the specified username
getUserInfo :: MonadIO m => Username -> RedditT m User
getUserInfo = runRoute . Route.aboutUser

-- | Get the listing of comments authored by the specified user.
getUserComments :: MonadIO m => Username -> RedditT m CommentListing
getUserComments = getUserComments' def

-- | Get the listing of comments authored by the specified user, with Options.
getUserComments' :: MonadIO m => Options CommentID -> Username -> RedditT m CommentListing
getUserComments' opts user = runRoute $ Route.userComments opts user

-- | Check whether the specified username is still available or has been taken.
isUsernameAvailable :: MonadIO m => Username -> RedditT m Bool
isUsernameAvailable = runRoute . Route.usernameAvailable

-- | Get information of the currently-logged-in user.
aboutMe :: MonadIO m => RedditT m User
aboutMe = runRoute Route.aboutMe

-- | Get users blocked by the currently-logged-in user.
getBlockedUsers :: MonadIO m => RedditT m [Relationship]
getBlockedUsers = do
  UserList rs <- runRoute Route.blocked
  return rs

-- | Get friends of the currently-logged-in user.
getFriends :: MonadIO m => RedditT m [Relationship]
getFriends = do
  UserList rs <- runRoute Route.friends
  return rs

-- | Check if a user has chosen (or been assign) user flair on a particular
--   subreddit. Requires moderator privileges on the specified subreddit.
lookupUserFlair :: MonadIO m => SubredditName -> Username -> RedditT m Flair
lookupUserFlair r u = do
  res <- liftM flistToListing $ runRoute (Route.lookupUserFlair r u)
  case res of
    Listing _ _ [f] -> return f
    _ -> failWith $ APIError InvalidResponseError

-- | Set a user's flair on the specified subreddit. Requires moderator
--   privileges on the specified subreddit.
setUserFlair :: MonadIO m => SubredditName -> Username -> Text -> Text -> RedditT m ()
setUserFlair r u txt cls =
  if Text.length txt > 64
    then fail "Flair text too long!"
    else nothing $ runRoute $ Route.setUserFlair r u txt cls
