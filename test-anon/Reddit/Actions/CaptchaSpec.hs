module Reddit.Actions.CaptchaSpec where

import Reddit
import Utils

import Test.Hspec

isRight :: Either a b -> Bool
isRight = const False `either` const True

main :: IO ()
main = hspec spec

spec :: Spec
spec = skip $ describe "Reddit.Actions.Captcha" $ do

  it "should be able to get a new captcha" $ do
    res <- runAnon newCaptcha
    res `shouldSatisfy` isRight

  it "should be able to check if we need captchas" $ do
    res <- runAnon needsCaptcha
    res `shouldSatisfy` isRight

skip :: Monad m => m a -> m ()
skip _ = return ()