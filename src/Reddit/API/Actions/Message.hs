module Reddit.API.Actions.Message where

import Reddit.API.Types.Empty
import Reddit.API.Types.Listing
import Reddit.API.Types.Message
import Reddit.API.Types.Reddit
import Reddit.API.Types.Thing
import Reddit.API.Types.User
import qualified Reddit.API.Routes.Message as Route

import APIBuilder
import Data.Text (Text)

getInbox :: Reddit (Listing Message)
getInbox = runRoute $ Route.inbox

getUnread :: Reddit (Listing Message)
getUnread = runRoute $ Route.unread

readMessage :: Thing a => a -> Reddit ()
readMessage = nothing . runRoute . Route.readMessage

sendMessage :: Username -> Text -> Text -> Reddit ()
sendMessage u s b = nothing $ runRoute $ Route.sendMessage u s b Nothing Nothing

sendMessageWithCaptcha :: Username -> Text -> Text -> Text -> Text -> Reddit ()
sendMessageWithCaptcha u s b i c = nothing $ runRoute $ Route.sendMessage u s b (Just i) (Just c)