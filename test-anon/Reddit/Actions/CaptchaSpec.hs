module Reddit.Actions.CaptchaSpec where

import Reddit

import Test.Hspec

isRight :: Either a b -> Bool
isRight = const False `either` const True

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
