module Reddit.Actions.Message
  ( getInbox
  , getUnread
  , markRead
  , sendMessage
  , sendMessageWithCaptcha ) where

import Reddit.Routes.Run
import Reddit.Types.Empty
import Reddit.Types.Listing
import Reddit.Types.Message
import Reddit.Types.Reddit
import Reddit.Types.Thing
import Reddit.Types.User
import qualified Reddit.Routes.Message as Route

import Control.Monad.IO.Class
import Data.Text (Text)
import Network.API.Builder.Query

-- | Get the message inbox for the current user.
getInbox :: MonadIO m => RedditT m (Listing MessageKind Message)
getInbox = runRoute Route.inbox

-- | Get any unread messages for the current user.
getUnread :: MonadIO m => RedditT m (Listing MessageKind Message)
getUnread = runRoute Route.unread

-- | Mark a message as read.
markRead :: (ToQuery a, Thing a, MonadIO m) => a -> RedditT m ()
markRead = nothing . runRoute . Route.readMessage

-- | Send a privatemessage to another user.
sendMessage :: MonadIO m => Username -> Text -> Text -> RedditT m ()
sendMessage u s b = nothing $ runRoute $ Route.sendMessage u s b Nothing Nothing

sendMessageWithCaptcha :: MonadIO m => Username -> Text -> Text -> Text -> Text -> RedditT m ()
sendMessageWithCaptcha u s b i c = nothing $ runRoute $ Route.sendMessage u s b (Just i) (Just c)
