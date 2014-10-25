module Reddit.API.Actions.CaptchaSpec where

import Reddit.API.Actions.Captcha

import ConfigLoad
import Data.Either
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.API.Actions.Captcha" $ do
  (reddit, _, _) <- runIO loadConfig

  it "should be able to get a new captcha" $ do
    res <- run reddit newCaptcha
    res `shouldSatisfy` isRight

  it "should be able to check if we need captchas" $ do
    res <- run reddit needsCaptcha
    res `shouldSatisfy` isRight
