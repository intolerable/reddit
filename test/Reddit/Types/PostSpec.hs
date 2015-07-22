module Reddit.Types.PostSpec where

import Reddit.Types.Post

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Test.Hspec

isLeft :: Either a b -> Bool
isLeft = const True `either` const False

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Types.Post" $ do
  let decode' = eitherDecode :: ByteString -> Either String PostID

  it "can parse a PostID" $ do
    decode' "\"t3_1n1qrg\"" `shouldBe` Right (PostID "1n1qrg")
    decode' "\"1n1qrg\"" `shouldBe` Right (PostID "1n1qrg")
    decode' "\"t5_2s580\"" `shouldSatisfy` isLeft
