module Reddit.Types.Captcha where

import Reddit.Types.Reddit

import Control.Applicative
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder
import Prelude

newtype CaptchaID = CaptchaID Text
  deriving (Read, Show, Eq, Ord)

instance FromJSON CaptchaID where
  parseJSON j = CaptchaID <$> parseJSON j

instance FromJSON (POSTWrapped CaptchaID) where
  parseJSON (Object o) =
    POSTWrapped <$> ((o .: "json") >>= (.: "data") >>= (.: "iden"))
  parseJSON _ = mempty

withCaptcha :: Route -> (CaptchaID, Text) -> Route
withCaptcha (Route pieces params meth) (CaptchaID i, c) =
  Route pieces (iden : captcha : params) meth
  where iden = ("iden" :: Text) =. i
        captcha = ("captcha" :: Text) =. c
