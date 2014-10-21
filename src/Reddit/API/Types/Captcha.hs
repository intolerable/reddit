module Reddit.API.Types.Captcha where

import Reddit.API.Types.Reddit

import Control.Applicative
import Data.Aeson
import Data.Monoid
import Data.Text (Text)

newtype CaptchaID = CaptchaID Text
  deriving (Read, Show, Eq, Ord)

instance FromJSON CaptchaID where
  parseJSON j = CaptchaID <$> parseJSON j

instance FromJSON (POSTWrapped CaptchaID) where
  parseJSON (Object o) =
    POSTWrapped <$> ((o .: "json") >>= (.: "data") >>= (.: "iden"))
  parseJSON _ = mempty
