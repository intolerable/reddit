module Reddit.API.Actions.User where

import Reddit.API.Routes.Run
import Reddit.API.Types
import Reddit.API.Types.Reddit
import qualified Reddit.API.Routes.User as Route

import Control.Monad.IO.Class

getUserInfo :: MonadIO m => Username -> RedditT m User
getUserInfo = runRoute . Route.aboutUser

getUserComments :: MonadIO m => Username -> RedditT m (Listing CommentID Comment)
getUserComments = runRoute . Route.userComments

isUsernameAvailable :: MonadIO m => Username -> RedditT m Bool
isUsernameAvailable = runRoute . Route.usernameAvailable

aboutMe :: MonadIO m => RedditT m User
aboutMe = runRoute Route.aboutMe
