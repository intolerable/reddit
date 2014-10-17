module Reddit.API.Actions.UserSpec where

import Reddit.API.Actions.User
import Reddit.API.Types.Comment
import Reddit.API.Types.Listing
import Reddit.API.Types.User

import ConfigLoad
import Control.Monad
import Data.DateTime
import Data.Either
import Data.Maybe
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.API.Actions.User" $ do
  (reddit, username, _) <- runIO loadConfig
  time <- runIO getCurrentTime

  it "should be able to get the user's most recent comments" $ do
    res <- run reddit $ getUserComments username
    res `shouldSatisfy` isRight
    case res of
      Left _ -> expectationFailure "something failed"
      Right (Listing _ _ cs) ->
        forM_ cs $ \c -> do
          author c `shouldBe` username
          replies c `shouldSatisfy` (\(Listing _ _ x) -> null x)
          created c `shouldSatisfy` (< time)

  it "should be able to get the user's about me info" $ do
    res <- run reddit aboutMe
    res `shouldSatisfy` isRight
    case res of
      Left _ -> expectationFailure "something failed"
      Right user -> do
        userName user `shouldBe` username
        userCreated user `shouldSatisfy` (< time)
        hasMail user `shouldSatisfy` isJust
        isFriend user `shouldBe` False

  it "should be able to get the user info for a user" $ do
    res <- run reddit $ getUserInfo username
    res `shouldSatisfy` isRight

  it "should be able to check if a username is available" $ do
    res <- run reddit $ isUsernameAvailable username
    res `shouldSatisfy` isRight
    case res of
      Left _ -> return ()
      Right avail -> avail `shouldBe` False
