module Reddit.API.Actions.User where

import qualified Reddit.API.Routes.User as Route
import Reddit.API.Types
import Reddit.API.Types.Reddit
import Reddit.API.Routes.Run

import Control.Monad.IO.Class

getUserInfo :: MonadIO m => Username -> RedditT m User
getUserInfo = runRoute . Route.aboutUser

isUsernameAvailable :: MonadIO m => Username -> RedditT m Bool
isUsernameAvailable = runRoute . Route.usernameAvailable

aboutMe :: MonadIO m => RedditT m User
aboutMe = runRoute Route.aboutMe
