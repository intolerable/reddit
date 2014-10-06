module Reddit.API.Types.UserSpec where

import Reddit.API.Types.User

import Data.Either
import Network.API.Builder
import Test.Hspec
import qualified Data.ByteString.Lazy.Char8 as ByteString

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Reddit.API.Types.User" $ do
    aboutMeExample <- runIO $ ByteString.readFile "test/data/aboutMe_example.json"
    getUserInfoExample <- runIO $ ByteString.readFile "test/data/getUserInfo_example.json"

    it "can read the examples" $ do
      aboutMeExample `shouldSatisfy` not . ByteString.null
      getUserInfoExample `shouldSatisfy` not . ByteString.null

    it "can parse a reddit user from json" $ do
      let decoded = decode aboutMeExample :: Either (APIError ()) User
      decoded `shouldSatisfy` isRight

      case decoded of
        Left _ -> expectationFailure "json parse failed"
        Right user -> do
          userID user `shouldBe` "4gf25"
          userName user `shouldBe` Username "Intolerable"
          linkKarma user `shouldBe` 2399
          commentKarma user `shouldBe` 44631
          hasMail user `shouldBe` Just False
          hasModMail user `shouldBe` Just False
          isFriend user `shouldBe` False
          userIsOver18 user `shouldBe` Just True
          isMod user `shouldBe` True
          hasGold user `shouldBe` False
          hasVerifiedEmail user `shouldBe` True

    it "can parse another reddit user from json" $ do
      let decoded = decode getUserInfoExample :: Either (APIError ()) User
      decoded `shouldSatisfy` isRight

      case decoded of
        Left _ -> expectationFailure "json parse failed"
        Right user -> do
          userID user `shouldBe` "gdifp"
          userName user `shouldBe` Username "intolerable-bot"
          linkKarma user `shouldBe` 1
          commentKarma user `shouldBe` 2668
          hasMail user `shouldBe` Nothing
          hasModMail user `shouldBe` Nothing
          isFriend user `shouldBe` False
          userIsOver18 user `shouldBe` Nothing
          isMod user `shouldBe` False
          hasGold user `shouldBe` False
          hasVerifiedEmail user `shouldBe` True
