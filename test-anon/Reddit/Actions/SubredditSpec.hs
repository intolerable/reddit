module Reddit.Actions.SubredditSpec where

import Reddit
import Reddit.Types.Subreddit
import Utils

import Test.Hspec

isRight :: Either a b -> Bool
isRight = const False `either` const True

isLeft :: Either a b -> Bool
isLeft = const True `either` const False

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Actions.Subreddit" $ do

  it "should be able to get the info for a subreddit anonymously" $ do
    let sub = R "gaming"
    res <- runAnon $ getSubredditInfo sub
    res `shouldSatisfy` isRight
    case res of
      Left _ -> expectationFailure "json parse failed"
      Right info -> do
        name info `shouldBe` sub
        subredditID info `shouldBe` SubredditID "2qh03"
        subscribers info `shouldSatisfy` (> 0)
        userIsBanned info `shouldBe` Nothing

  it "shouldn't be able to anonymously get the settings for the moderated subreddit" $ do
    res <- runAnon $ getSubredditSettings $ R "gaming"
    res `shouldSatisfy` isLeft

