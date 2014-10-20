module Reddit.API.Actions.PostSpec where

import Reddit.API.Actions.Post
import Reddit.API.Types.Listing
import Reddit.API.Types.Post
import Reddit.API.Types.Subreddit (SubredditID(..))
import Reddit.API.Types.User

import ConfigLoad
import Data.Either
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.API.Actions.Post" $ do
  (reddit, _, _) <- runIO loadConfig

  it "should be able to get info for a post" $ do
    res <- run reddit $ getPostInfo (PostID "z1c9z")
    res `shouldSatisfy` isRight
    case res of
      Left _ -> expectationFailure "something failed"
      Right post -> do
        author post `shouldBe` Username "PresidentObama"
        title post `shouldBe` "I am Barack Obama, President of the United States -- AMA"
        subredditID post `shouldBe` SubredditID "2qzb6"
        nsfw post `shouldBe` False

  it "should be able to get info for multiple posts" $ do
    res <- run reddit $ getPostsInfo [PostID "z1c9z", PostID "t0ynr"]
    res `shouldSatisfy` isRight

  it "should cope with getting info for no posts" $ do
    res <- run reddit $ getPostsInfo []
    res `shouldSatisfy` isRight
    case res of
      Left _ -> expectationFailure "something failed"
      Right (Listing _ _ ps) ->
        ps `shouldBe` []
