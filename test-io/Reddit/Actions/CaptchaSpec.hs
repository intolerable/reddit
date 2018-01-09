module Reddit.Actions.CaptchaSpec where

import Reddit.Actions.Captcha

import ConfigLoad
import Test.Hspec

isRight :: Either a b -> Bool
isRight = const False `either` const True

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Actions.Captcha" $ do
  (reddit, _, _) <- runIO loadConfig

  it "should be able to check if we need captchas" $ do
    res <- run reddit needsCaptcha
    res `shouldSatisfy` isRight
