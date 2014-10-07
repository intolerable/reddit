module Reddit.API.Actions.User where

import Reddit.API.Routes.Run
import Reddit.API.Types.User
import Reddit.API.Types.Comment
import Reddit.API.Types.Options
import Reddit.API.Types.Reddit
import qualified Reddit.API.Routes.User as Route

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
