module Reddit.API.Routes.User where

import Reddit.API.Types.User

import APIBuilder.Routes

aboutUser :: Username -> Route
aboutUser (Username user) = Route [ "user", user, "about.json" ]
                                  []
                                  "GET"

aboutMe :: Route
aboutMe = Route [ "api", "me.json" ]
                []
                "GET"

userComments :: Username -> Route
userComments (Username user) =
  Route [ "user", user ]
        [ "limit" =. (100 :: Int) ]
        "GET"

usernameAvailable :: Username -> Route
usernameAvailable user = Route [ "api", "username_available.json" ]
                                          [ "user" =. user]
                                          "GET"

