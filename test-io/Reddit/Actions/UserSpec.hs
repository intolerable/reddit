module Reddit.Actions.UserSpec where

import Reddit.Actions.User
import Reddit.Types.Comment
import Reddit.Types.Listing
import Reddit.Types.Options
import Reddit.Types.User

import ConfigLoad
import Control.Monad
import Data.Either
import Data.Maybe
import Data.Time.Clock
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Actions.User" $ do
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

  it "should be able to get multiple pages of user comments" $ do
    res <- run reddit $ getUserComments' (Options Nothing (Just 1)) username
    res `shouldSatisfy` isRight
    case res of
      Right (Listing _ (Just a) (c:[])) -> do
        next <- run reddit $ getUserComments' (Options (Just $ After a) (Just 1)) username
        next `shouldSatisfy` isRight
        case next of
          Right (Listing _ _ (d:[])) ->
            c `shouldSatisfy` (/= d)
          _ -> expectationFailure "something failed"
      _ -> expectationFailure "something failed"

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
