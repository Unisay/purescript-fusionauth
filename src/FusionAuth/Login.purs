module FusionAuth.Login 
  ( LoginRequestRep
  , LoginRequest (..)
  , LoginResponseF (..)
  , LoginResponse
  , defaultLoginRequest
  , encodeLoginRequest
  , decodeLoginResponse
  ) where

import Prelude

import Data.Argonaut (class EncodeJson, Json, decodeJson, encodeJson, jsonEmptyObject, (.:), (.:?), (:=), (:=?), (~>), (~>?))
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import FusionAuth.Data.ApplicationId (ApplicationId)
import FusionAuth.Data.Email (Email)
import FusionAuth.Data.Password (Password)
import FusionAuth.Data.Token (Token)
import FusionAuth.Data.User (User, decodeUser)
import FusionAuth.Data.Username (Username)

-- | https://fusionauth.io/docs/v1/tech/apis/login#request
type LoginRequestRep r =
  ( applicationId :: Maybe ApplicationId
  , noJWT :: Maybe Boolean
  , loginId :: Either Email Username
  , password :: Either Password Token
  | r
  )

type LoginRequest = {| LoginRequestRep ()}

encodeLoginRequest :: LoginRequest -> Json
encodeLoginRequest r
      = "loginId" := or r.loginId
     ~> "password" := or r.password
     ~> "applicationId" :=? r.applicationId
    ~>? "noJWT" :=? r.noJWT
    ~>? jsonEmptyObject
     where 
     or :: forall a b. EncodeJson a => EncodeJson b => Either a b -> Json
     or = either encodeJson encodeJson
    
defaultLoginRequest :: Email -> Password -> LoginRequest
defaultLoginRequest email password =
  { loginId: Left email
  , password: Left password
  , applicationId: Nothing
  , noJWT: Nothing 
  }


data LoginResponseF a
  = LoginUnsuccessful
  | LoginSuccessful a

type LoginResponse 
  = LoginResponseF 
    { token :: Token
    , refreshToken :: Maybe Token
    , user :: User
    }

instance foldableLoginResponse :: Foldable LoginResponseF where
  foldr f b (LoginSuccessful a) = f a b 
  foldr _ b _ = b

  foldl f b (LoginSuccessful a) = f b a
  foldl _ b _ = b

  foldMap f (LoginSuccessful a) = f a
  foldMap _ _ = mempty

decodeLoginResponse :: Json -> Either String LoginResponse
decodeLoginResponse json = do
  x <- decodeJson json
  token <- x .: "token"
  refreshToken <- x .:? "refreshToken"
  user <- x .: "user" >>= decodeUser
  pure $ LoginSuccessful { token, refreshToken, user }