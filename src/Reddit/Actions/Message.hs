module Reddit.Actions.Message
  ( getInbox
  , getInbox'
  , getUnread
  , getUnread'
  , markRead
  , sendMessage
  , sendMessageWithCaptcha ) where

import Reddit.Routes.Run
import Reddit.Types.Captcha
import Reddit.Types.Empty
import Reddit.Types.Listing
import Reddit.Types.Message
import Reddit.Types.Options
import Reddit.Types.Reddit
import Reddit.Types.Thing
import Reddit.Types.User
import qualified Reddit.Routes.Message as Route

import Control.Monad.IO.Class
import Data.Default
import Data.Text (Text)
import Network.API.Builder.Query

-- | Get the message inbox for the current user.
getInbox :: MonadIO m => RedditT m (Listing MessageKind Message)
getInbox = runRoute $ Route.inbox False def

-- | Don't use this for watching for new messages, Reddit's ordering on
-- |   inbox messages is odd and not likely to work how you expect.
getInbox' :: MonadIO m => Bool -> Options MessageKind -> RedditT m (Listing MessageKind Message)
getInbox' m o = runRoute $ Route.inbox m o

-- | Get any unread messages for the current user.
getUnread :: MonadIO m => RedditT m (Listing MessageKind Message)
getUnread = runRoute $ Route.unread False def

getUnread' :: MonadIO m => Bool -> Options MessageKind -> RedditT m (Listing MessageKind Message)
getUnread' m o = runRoute $ Route.unread m o

-- | Mark a message as read.
markRead :: (ToQuery a, Thing a, MonadIO m) => a -> RedditT m ()
markRead = nothing . runRoute . Route.readMessage

-- | Send a private message to another user.
sendMessage :: MonadIO m => Username -> Text -> Text -> RedditT m ()
sendMessage u s b = nothing $ runRoute $ Route.sendMessage u s b

-- | Send a private message (with a captcha).
sendMessageWithCaptcha :: MonadIO m => Username -> Text -> Text -> CaptchaID -> Text -> RedditT m ()
sendMessageWithCaptcha u s b i c = nothing $ runRoute $ Route.sendMessage u s b `withCaptcha` (i, c)
