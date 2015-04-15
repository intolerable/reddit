module Reddit.Types.CommentSpec where

import Reddit.Types.Comment
import Reddit.Types.Listing
import Reddit.Types.Subreddit hiding (subredditID)
import Reddit.Types.User

import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.Either
import Data.Maybe
import Data.Time.Clock
import Network.API.Builder
import Test.Hspec
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text as Text

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Types.Comment" $ do
  let decode' = eitherDecode :: ByteString -> Either String CommentID
  getUserCommentsExample <- runIO $ ByteString.readFile "test/data/getUserComments_example.json"
  time <- runIO getCurrentTime

  it "can read the example" $
    getUserCommentsExample `shouldSatisfy` not . ByteString.null

  it "can parse a list of comments from json" $ do
    let decoded = eitherDecode getUserCommentsExample :: Either String CommentListing
    decoded `shouldSatisfy` isRight

    case decoded of
      Left _ -> expectationFailure "json parse failed"
      Right (Listing b a cs) -> do
        cs `shouldSatisfy` not . null
        b `shouldBe` Nothing
        a `shouldSatisfy` isJust

        length cs `shouldBe` 5
        forM_ cs $ \c -> do
          author c `shouldBe` Username "Intolerable"
          score c `shouldSatisfy` isJust
          subreddit c `shouldBe` R "DotA2"
          subredditID c `shouldBe` SubredditID "2s580"
          authorFlairText c `shouldBe` Just "sneep sneep"
          authorFlairCSSClass c `shouldBe` Just "rapier"
          body c `shouldSatisfy` not . Text.null
          bodyHTML c `shouldSatisfy` not . Text.null
          replies c `shouldBe` Listing Nothing Nothing []
          created c `shouldSatisfy` (< time)

  it "can parse a CommentID" $ do
    decode' "\"t1_cl1royq\"" `shouldBe` Right (CommentID "cl1royq")
    decode' "\"cl1royq\"" `shouldBe` Right (CommentID "cl1royq")
    decode' "\"t5_2s580\"" `shouldSatisfy` isLeft
