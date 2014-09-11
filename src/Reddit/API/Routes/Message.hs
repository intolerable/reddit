module Reddit.API.Routes.Message where

import Reddit.API.Types.Thing
import Reddit.API.Types.User

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

sendMessage :: Username -> Text -> Text -> Maybe Text -> Maybe Text -> Route
sendMessage (Username u) subject body iden captcha =
  Route [ "api", "compose" ]
        [ "to" =. u
        , "subject" =. subject
        , "text" =. body
        , "iden" =. iden
        , "captcha" =. captcha ]
        "POST"
