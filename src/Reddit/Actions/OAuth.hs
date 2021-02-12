module Reddit.Actions.OAuth
  ( accessTokenWithCode
  , accessTokenWithRefreshToken
  ) where

import Data.ByteString.Base64 (encode)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Network.API.Builder hiding (runRoute)
import Network.HTTP.Types (Header)
import Reddit.Types.OAuth
import Reddit.Types.Reddit

accessTokenWithCodeRoute :: AuthorizationCode -> Client -> Route
accessTokenWithCodeRoute AuthorizationCode {authorizationCode = code} client =
  Route
    ["api", "v1", "access_token"]
    [ ("grant_type" :: Text) =. ("authorization_code" :: Text)
    , "code" =. code
    , "redirect_uri" =. redirectUrl client
    ]
    "POST"

accessTokenWithCode ::
     Monad m
  => AuthorizationCode
  -> Client
  -> RedditT m (AccessToken, RefreshToken)
accessTokenWithCode code client =
  withHeaders (authorizationHeader client :) $
  receiveRoute $ accessTokenWithCodeRoute code client

accessTokenWithRefreshTokenRoute :: RefreshToken -> Route
accessTokenWithRefreshTokenRoute RefreshToken {refreshToken = token} =
  Route
    ["api", "v1", "access_token"]
    [ ("grant_type" :: Text) =. ("refresh_token" :: Text)
    , "refresh_token" =. token
    ]
    "POST"

accessTokenWithRefreshToken ::
     Monad m => RefreshToken -> Client -> RedditT m AccessToken
accessTokenWithRefreshToken refreshToken client =
  withHeaders (authorizationHeader client :) $
  receiveRoute $ accessTokenWithRefreshTokenRoute refreshToken

authorizationHeader :: Client -> Header
authorizationHeader Script {clientId = clientId, secret = secret} =
  ( "Authorization"
  , "Basic " <> encode (Text.encodeUtf8 (clientId <> ":" <> secret)))
