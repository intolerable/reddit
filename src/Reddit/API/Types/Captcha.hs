module Reddit.API.Types.Captcha where

import Control.Applicative
import Data.Aeson
import Data.Text (Text)

newtype CaptchaID = CaptchaID Text
  deriving (Read, Show, Eq, Ord)

instance FromJSON CaptchaID where
  parseJSON j = CaptchaID <$> parseJSON j
