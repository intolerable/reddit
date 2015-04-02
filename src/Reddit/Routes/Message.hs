module Reddit.Routes.Message where

import Reddit.Types.Thing
import Reddit.Types.User

import Data.Text (Text)
import Network.API.Builder.Routes

inbox :: Route
inbox = Route [ "message", "inbox" ]
              []
              "GET"

unread :: Route
unread = Route [ "message", "unread" ]
               []
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
