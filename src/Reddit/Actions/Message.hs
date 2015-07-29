-- | Contains message-related actions, like retrieving your own inbox
--   and sending other users private messages.
module Reddit.Actions.Message
  ( getInbox
  , getInbox'
  , getUnread
  , getUnread'
  , markRead
  , sendMessage
  , sendMessageWithCaptcha
  , replyMessage ) where

import Reddit.Types.Captcha
import Reddit.Types.Empty
import Reddit.Types.Listing
import Reddit.Types.Message
import Reddit.Types.Options
import Reddit.Types.Reddit
import Reddit.Types.Thing
import Reddit.Types.User
import qualified Reddit.Routes.Message as Route
import qualified Reddit.Routes.Thing as Route

import Data.Default.Class
import Data.Text (Text)
import Network.API.Builder.Query

-- | Get the message inbox for the current user.
getInbox :: Monad m => RedditT m (Listing MessageKind Message)
getInbox = runRoute $ Route.inbox False def

-- | Don't use this for watching for new messages, Reddit's ordering on
-- |   inbox messages is odd and not likely to work how you expect.
getInbox' :: Monad m => Bool -> Options MessageKind -> RedditT m (Listing MessageKind Message)
getInbox' m o = runRoute $ Route.inbox m o

-- | Get any unread messages for the current user.
getUnread :: Monad m => RedditT m (Listing MessageKind Message)
getUnread = runRoute $ Route.unread False def

-- | Get unread messages for the current user, with options.
getUnread' :: Monad m
           => Bool -- ^ Whether the orangered notifier should be marked "off"
           -> Options MessageKind
           -> RedditT m (Listing MessageKind Message)
getUnread' m o = runRoute $ Route.unread m o

-- | Mark a message as read.
markRead :: (ToQuery a, Thing a, Monad m) => a -> RedditT m ()
markRead = nothing . runRoute . Route.readMessage

-- | Send a private message to another user.
sendMessage :: Monad m
            => Username -- ^ The username to send the message to
            -> Text -- ^ The subject of the message being sent
            -> Text -- ^ The body of the message being sent
            -> RedditT m ()
sendMessage u s b = nothing $ runRoute $ Route.sendMessage u s b

-- | Send a private message (with a captcha).
sendMessageWithCaptcha :: Monad m
                       => Username -- ^ The username to send the message to
                       -> Text -- ^ The subject of the message being sent
                       -> Text -- ^ The body of the message being sent
                       -> CaptchaID -- ^ The identifier of the captcha being answered
                       -> Text -- ^ The answer to the specified captcha
                       -> RedditT m ()
sendMessageWithCaptcha u s b i c = nothing $ runRoute $ Route.sendMessage u s b `withCaptcha` (i, c)

-- | Reply to a message
replyMessage :: (Monad m, Thing a)
             => a -- ^ Thing to reply to
             -> Text -- ^ Response contents
             -> RedditT m MessageID
replyMessage t b = do
  POSTWrapped res <- runRoute $ Route.reply t b
  return res
