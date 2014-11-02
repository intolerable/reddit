module Reddit.API.Actions.Captcha
  ( needsCaptcha
  , newCaptcha ) where

import Reddit.API.Routes.Run
import Reddit.API.Types.Captcha
import Reddit.API.Types.Reddit
import qualified Reddit.API.Routes.Captcha as Route

import Control.Monad.IO.Class

-- | Find out if the account currently logged in requires a captcha to be submitted for
--   certain requests (like sending a private message or submitting a post).
needsCaptcha :: MonadIO m => RedditT m Bool
needsCaptcha = runRoute Route.needsCaptcha

-- | Returns the ID of a captcha to be completed (the image for which can be found at
--   <http://reddit.com/captcha/$CAPTCHA_ID>)
newCaptcha :: MonadIO m => RedditT m CaptchaID
newCaptcha = do
  POSTWrapped c <- runRoute Route.newCaptcha
  return c
