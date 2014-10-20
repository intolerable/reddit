module Reddit.API.Actions.CommentSpec where

import Reddit.API.Actions.Comment
import Reddit.API.Types.Comment
import Reddit.API.Types.Listing

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

  it "should be able to get info for multiple comments" $ do
    res <- run reddit $ getCommentsInfo [CommentID "c60o0iw", CommentID "c4zrenn"]
    res `shouldSatisfy` isRight

  it "should fail if we try to get an invalid comment" $ do
    res <- run reddit $ getCommentInfo (CommentID "garbage")
    res `shouldSatisfy` isLeft

  it "should be able to get a list of comment IDs where some are invalid" $ do
    res <- run reddit $ getCommentsInfo [CommentID "c60o0iw", CommentID "garbage"]
    res `shouldSatisfy` isRight
    case res of
      Left _ -> expectationFailure "something failed"
      Right (Listing _ _ cs) -> do
        length cs `shouldBe` 1
