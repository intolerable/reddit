module Reddit.Routes.Message where

import Reddit.Types.Options
import Reddit.Types.Message
import Reddit.Types.Thing
import Reddit.Types.User

import Data.Text (Text)
import Network.API.Builder.Routes

inbox :: Bool -> Options MessageKind -> Route
inbox shouldMark opts =
  Route [ "message", "inbox" ]
        [ "mark" =. shouldMark
        , "before" =. before opts
        , "after" =. after opts
        , "limit" =. limit opts ]
        "GET"

unread :: Bool -> Options MessageKind -> Route
unread shouldMark opts =
  Route [ "message", "unread" ]
        [ "mark" =. shouldMark
        , "before" =. before opts
        , "after" =. after opts
        , "limit" =. limit opts ]
        "GET"

readMessage :: Thing a => a -> Route
readMessage m = Route [ "api", "read_message" ]
                      [ "id" =. fullName m ]
                      "POST"

sendMessage :: Username -> Text -> Text -> Route
sendMessage (Username u) subject body =
  Route [ "api", "compose" ]
        [ "to" =. u
        , "subject" =. subject
        , "text" =. body ]
        "POST"
