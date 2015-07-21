module Reddit.Actions.UserSpec where

import Reddit
import Reddit.Types.Comment

import Control.Monad
import Data.Time.Clock
import Test.Hspec

isRight :: Either a b -> Bool
isRight = const False `either` const True

isLeft :: Either a b -> Bool
isLeft = const True `either` const False

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Actions.User" $ do
  time <- runIO getCurrentTime

  it "should be able to get the user's most recent comments" $ do
    res <- runRedditAnon $ getUserComments $ Username "Intolerable"
    res `shouldSatisfy` isRight
    case res of
      Left _ -> expectationFailure "something failed"
      Right (Listing _ _ cs) ->
        forM_ cs $ \c -> do
          author c `shouldBe` Username "Intolerable"
          replies c `shouldSatisfy` (\(Listing _ _ x) -> null x)
          created c `shouldSatisfy` (< time)

  it "should be able to get multiple pages of user comments" $ do
    res <- runRedditAnon $ getUserComments' (Options Nothing (Just 1)) $ Username "Intolerable"
    res `shouldSatisfy` isRight
    case res of
      Right (Listing _ (Just a) (c:[])) -> do
        next <- runRedditAnon $ getUserComments' (Options (Just $ After a) (Just 1)) $ Username "Intolerable"
        next `shouldSatisfy` isRight
        case next of
          Right (Listing _ _ (d:[])) ->
            c `shouldSatisfy` (/= d)
          _ -> expectationFailure "something failed"
      _ -> expectationFailure "something failed"

  it "shouldn't be able to get about me info for an anonymous user" $ do
    res <- runRedditAnon aboutMe
    res `shouldSatisfy` isLeft

  it "should be able to anonymously get the user info for a user" $ do
    res <- runRedditAnon $ getUserInfo $ Username "Intolerable"
    res `shouldSatisfy` isRight

  it "should be able to check if a username is available" $ do
    res <- runRedditAnon $ isUsernameAvailable $ Username "Intolerable"
    res `shouldSatisfy` isRight
    case res of
      Left _ -> return ()
      Right avail -> avail `shouldBe` False

