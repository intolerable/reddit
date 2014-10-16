module Reddit.API.Actions.UserSpec where

import Reddit.API.Actions.User

import ConfigLoad
import Data.Either
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.API.Actions.User" $ do
  (reddit, username, _) <- runIO loadConfig

  it "should be able to get the user's most recent comments" $ do
    res <- run reddit $ getUserComments username
    res `shouldSatisfy` isRight

  it "should be able to get the user's about me info" $ do
    res <- run reddit $ aboutMe
    res `shouldSatisfy` isRight

  it "should be able to get the user info for a user" $ do
    res <- run reddit $ getUserInfo username
    res `shouldSatisfy` isRight

  it "should be able to check if a username is available" $ do
    res <- run reddit $ isUsernameAvailable username
    res `shouldSatisfy` isRight
    case res of
      Left _ -> return ()
      Right avail -> avail `shouldBe` False
