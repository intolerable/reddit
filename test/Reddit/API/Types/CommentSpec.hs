module Reddit.API.Types.CommentSpec where

import Reddit.API.Types.Comment
import Reddit.API.Types.Listing
import Reddit.API.Types.User

import Control.Monad
import Data.DateTime
import Data.Either
import Data.Maybe
import Network.API.Builder
import Test.Hspec
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text as Text

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.API.Types.Comment" $ do
  getUserCommentsExample <- runIO $ ByteString.readFile "test/data/getUserComments_example.json"
  time <- runIO getCurrentTime

  it "can read the example" $
    getUserCommentsExample `shouldSatisfy` not . ByteString.null

  it "can parse a list of comments from json" $ do
    let decoded = decode getUserCommentsExample :: Either (APIError ()) CommentListing
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
          authorFlairText c `shouldBe` Just "sneep sneep"
          authorFlairCSSClass c `shouldBe` Just "rapier"
          body c `shouldSatisfy` not . Text.null
          bodyHTML c `shouldSatisfy` not . Text.null
          replies c `shouldBe` Listing Nothing Nothing []
          created c `shouldSatisfy` (< time)
