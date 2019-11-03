module Reddit.Types.OAuth where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text as Text
import Network.API.Builder

data Client =
  Script
    { clientId :: Text
    , secret :: Text
    , redirectUrl :: Text
    }
  deriving (Show)

newtype RefreshToken =
  RefreshToken
    { refreshToken :: Text
    }
  deriving (Show)

instance Receivable RefreshToken where
  receive = useFromJSON

instance FromJSON RefreshToken where
  parseJSON (Object o) = RefreshToken <$> (o .: "refresh_token")

newtype AuthorizationCode =
  AuthorizationCode
    { authorizationCode :: Text
    }
  deriving (Show)

data TokenType =
  Bearer
  deriving (Show, Enum)

instance FromJSON TokenType where
  parseJSON (String raw) =
    case lookup raw $
         Prelude.map (\elem -> (toLower $ pack $ show elem, elem)) [Bearer ..] of
      Just result -> return result
      Nothing -> fail $ "Unsupported token type in response: " ++ unpack raw

data AccessToken =
  AccessToken
    { accessToken :: Text
    , tokenType :: TokenType
    , expiresIn :: Word
    , scope :: [Scope]
    }
  deriving (Show)

instance FromJSON AccessToken where
  parseJSON (Object o) =
    AccessToken <$> (o .: "access_token") <*> (o .: "token_type") <*>
    (o .: "expires_in") <*>
    (o .: "scope" >>= parseJSONScopeString)

instance Receivable AccessToken where
  receive = useFromJSON

data Scope
  = Identity
  | Edit
  | Flair
  | History
  | ModConfig
  | ModFlair
  | ModLog
  | ModPosts
  | ModWiki
  | MySubreddits
  | PrivateMessages
  | Read
  | Report
  | Save
  | Submit
  | Subscribe
  | Vote
  | WikiEdit
  | WikiRead
  | All
  deriving (Show, Enum)

parseJSONScopeString :: Value -> Parser [Scope]
parseJSONScopeString (String text) =
  Prelude.mapM textToScope $
  split
    (\c ->
       case c of
         ' ' -> True
         ',' -> True
         _ -> False)
    text
  where
    textToScope "*" = return All
    textToScope raw =
      case lookup raw $
           Prelude.map
             (\elem -> (toLower $ pack $ show elem, elem))
             [Identity ..] of
        Just result -> return result
        Nothing -> fail $ "Unsupported token type in response: " ++ unpack raw
