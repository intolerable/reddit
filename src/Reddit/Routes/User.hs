module Reddit.Routes.User where

import Reddit.Types.Comment (CommentID)
import Reddit.Types.Options
import Reddit.Types.Subreddit
import Reddit.Types.User

import Data.Text (Text)
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

blocked :: Route
blocked = Route [ "prefs", "blocked" ]
                [ ]
                "GET"

friends :: Route
friends = Route [ "prefs", "friends" ]
                [ ]
                "GET"

lookupUserFlair :: SubredditName -> Username -> Route
lookupUserFlair (R r) u =
  Route [ "r", r, "api", "flairlist" ]
        [ "name" =. u ]
        "GET"

setUserFlair :: SubredditName -> Username -> Text -> Text -> Route
setUserFlair (R r) u txt cls =
  Route [ "r", r, "api", "flair" ]
        [ "name" =. u
        , "text" =. txt
        , "css_class" =. cls ]
        "POST"
