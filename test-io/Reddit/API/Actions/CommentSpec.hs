module Reddit.API.Actions.CommentSpec where

import Reddit.API.Actions.Comment
import Reddit.API.Types.Comment

import ConfigLoad
import Data.Either
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.API.Actions.Comment" $ do
  (reddit, _, _) <- runIO loadConfig

  it "should be able to get info for a comment" $ do
    res <- run reddit $ getCommentInfo (CommentID "c60o0iw")
    res `shouldSatisfy` isRight
