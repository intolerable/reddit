module Reddit.Routes.Captcha where

import Reddit.Types.Captcha

import Network.API.Builder.Routes

needsCaptcha :: Route
needsCaptcha = Route [ "api", "needs_captcha.json" ]
                     [ ]
                     "GET"

newCaptcha :: Route
newCaptcha = Route [ "api", "new_captcha" ]
                   [ ]
                   "POST"

getCaptcha :: CaptchaID -> Route
getCaptcha (CaptchaID c) =
  Route [ "captcha", c ]
        [ ]
        "GET"
