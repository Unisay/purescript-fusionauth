module FusionAuth.Login 
  ( LoginRequestRep
  , LoginRequest (..)
  , LoginResponse (..)
  , defaultLoginRequest
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonEmptyObject, (.:), (.:?), (:=), (:=?), (~>), (~>?))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import FusionAuth.Data.ApplicationId (ApplicationId)
import FusionAuth.Data.Email (Email)
import FusionAuth.Data.Password (Password)
import FusionAuth.Data.Token (Token)
import FusionAuth.Data.User (UserIn)
import FusionAuth.Data.Username (Username)

-- | https://fusionauth.io/docs/v1/tech/apis/login#request
type LoginRequestRep r =
  ( applicationId :: Maybe ApplicationId
  , noJWT :: Maybe Boolean
  , loginId :: Either Email Username
  , password :: Either Password Token
  | r
  )

newtype LoginRequest = LoginRequest {| LoginRequestRep ()}

derive instance newtypeLoginRequest :: Newtype LoginRequest _

instance encodeJsonLoginRequestNT :: EncodeJson LoginRequest where
  encodeJson (LoginRequest u) 
      = "loginId" := or u.loginId
     ~> "password" := or u.password
     ~> "applicationId" :=? u.applicationId
    ~>? "noJWT" :=? u.noJWT
    ~>? jsonEmptyObject
     where 
     or :: forall a b. EncodeJson a => EncodeJson b => Either a b -> Json
     or = either encodeJson encodeJson
    
defaultLoginRequest :: Email -> Password -> LoginRequest
defaultLoginRequest email password =
  LoginRequest
  { loginId: Left email
  , password: Left password
  , applicationId: Nothing
  , noJWT: Nothing 
  }



newtype LoginResponse =
  LoginResponse
  { token :: Token
  , refreshToken :: Maybe Token
  , user :: UserIn
  }

derive instance newtypeLoginResponse :: Newtype LoginResponse _

instance decodeJsonLoginResponse :: DecodeJson LoginResponse where
  decodeJson json = do
    x <- decodeJson json
    token <- x .: "token"
    refreshToken <- x .:? "refreshToken"
    user <- x .: "user"
    pure $ LoginResponse { token, refreshToken, user }