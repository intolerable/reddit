module Reddit.API.Actions.User where

import qualified Reddit.API.Routes.User as Route
import Reddit.API.Types

import APIBuilder

getUserInfo :: Username -> Reddit User
getUserInfo = runRoute . Route.aboutUser

isUsernameAvailable :: Username -> Reddit Bool
isUsernameAvailable = runRoute . Route.usernameAvailable

aboutMe :: Reddit User
aboutMe = runRoute Route.aboutMe