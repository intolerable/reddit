module Reddit.Routes.User where

import Reddit.Types.Comment (CommentID)
import Reddit.Types.Options
import Reddit.Types.User

import Network.API.Builder.Routes

aboutUser :: Username -> Route
aboutUser (Username user) = Route [ "user", user, "about.json" ]
                                  []
                                  "GET"

aboutMe :: Route
aboutMe = Route [ "api", "me.json" ]
                []
                "GET"

userComments :: Options CommentID -> Username -> Route
userComments opts (Username user) =
  Route [ "user", user, "comments" ]
        [ "limit" =. limit opts
        , "before" =. before opts
        , "after" =. after opts ]
        "GET"

usernameAvailable :: Username -> Route
usernameAvailable user = Route [ "api", "username_available.json" ]
                               [ "user" =. user]
                               "GET"

