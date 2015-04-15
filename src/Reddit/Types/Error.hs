module Reddit.Types.Error
  (RedditError(..)) where

import Control.Applicative
import Data.Aeson
import Data.Monoid (mempty)
import Data.Text (Text)
import Data.Vector ((!?))
import Network.API.Builder.Receive
import Prelude hiding (mempty)
import qualified Data.Vector as V

data RedditError = RedditError Object
                 | FailError Text
                 | InvalidResponseError
                 | CaptchaError Text
                 | CredentialsError
                 | RateLimitError Integer Text
                 | NoSubredditSpecified
                 | NoURLSpecified
                 | NoName
                 | NoText Text
                 | AlreadySubmitted
                 | CommentDeleted
                 | LinkDeleted
                 | BadSubredditName
                 | TooManyRequests
                 deriving (Show, Eq)

instance FromJSON RedditError where
  parseJSON (Object o) = do
    Array errors <- o .: "json" >>= (.: "errors")
    case errors !? 0 of
      Just (Array e) -> case V.toList e of
        String "WRONG_PASSWORD" : _ -> return CredentialsError
        String "USER_REQUIRED" : _ -> return CredentialsError
        String "RATELIMIT" : String d : _ ->
            RateLimitError <$> ((o .: "json") >>= (.: "ratelimit")) <*> pure d
        String "SUBREDDIT_REQUIRED" : _ -> return NoSubredditSpecified
        String "ALREADY_SUB" : _ -> return AlreadySubmitted
        String "NO_URL" : _ -> return NoURLSpecified
        String "NO_NAME" : _ -> return NoName
        String "NO_TEXT" : _ : String f : _ -> return $ NoText f
        String "COMMENT_DELETED" : _ -> return CommentDeleted
        String "DELETED_LINK" : _ -> return LinkDeleted
        String "BAD_SR_NAME" : _ -> return BadSubredditName
        String "BAD_CAPTCHA" : _ -> CaptchaError <$> (o .: "json" >>= (.: "captcha"))
        _ -> return $ RedditError o
      _ -> mempty
  parseJSON _ = mempty

instance ErrorReceivable RedditError where
  receiveError = useErrorFromJSON
