module Reddit.API.Routes.User where

import Reddit.API.Types.User
import APIBuilder.Routes

aboutUser :: Username -> Route
aboutUser (Username user) = Route [ "user", user, "about.json" ]
                                  []
                                  GET

aboutMe :: Route
aboutMe = Route [ "api", "me.json" ]
                []
                GET

usernameAvailable :: Username -> Route
usernameAvailable (Username user) = Route [ "api", "username_available.json" ]
                                          [ "user" =. Just user]
                                          GET

