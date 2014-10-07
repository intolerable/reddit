module Reddit.API.Routes.User where

import Reddit.API.Types.Comment (CommentID)
import Reddit.API.Types.Options
import Reddit.API.Types.User

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

