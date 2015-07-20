module Reddit.Actions.CaptchaSpec where

import Reddit

import Data.Either
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Actions.Captcha" $ do

  it "should be able to get a new captcha" $ do
    res <- runRedditAnon newCaptcha
    res `shouldSatisfy` isRight

  it "should be able to check if we need captchas" $ do
    res <- runRedditAnon needsCaptcha
    res `shouldSatisfy` isRight
