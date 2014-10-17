module Reddit.API.Actions.PostSpec where

import Reddit.API.Actions.Post
import Reddit.API.Types.Post

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

  it "should be able to get info for multiple posts" $ do
    res <- run reddit $ getPostsInfo [PostID "z1c9z", PostID "t0ynr"]
    res `shouldSatisfy` isRight
