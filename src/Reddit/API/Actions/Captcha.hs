module Reddit.API.Actions.Captcha where

import Reddit.API.Routes.Run
import Reddit.API.Types.Captcha
import Reddit.API.Types.Reddit
import qualified Reddit.API.Routes.Captcha as Route

import Control.Monad.IO.Class

needsCaptcha :: MonadIO m => RedditT m Bool
needsCaptcha = runRoute Route.needsCaptcha

newCaptcha :: MonadIO m => RedditT m CaptchaID
newCaptcha = runRoute Route.newCaptcha
