module FusionAuth.Register
  ( RegisterRequestRep
  , RegisterRequest (..)
  , RegisterResponse (..)
  , defaultRegisterRequest
  ) where

import Data.Argonaut (class DecodeJson, class EncodeJson, jsonEmptyObject, (:=), (:=?), (~>), (~>?))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import FusionAuth.Data.Registration (RegistrationIn, RegistrationOut)
import FusionAuth.Data.TwoFactorDelivery (TwoFactorDelivery)
import FusionAuth.Data.User (UserIn, UserOut)

type RegisterRequestRep f r =
  -- | Determines if FusionAuth should generate an 
  -- | Authentication Token for this registration. 
  ( generateAuthenticationToken :: f Boolean

  -- | Whether or not the User is sent an email asking them 
  -- | to setup their password. This is useful if someone else 
  -- | is creating an account for a User with an Application. 
  , sendSetPasswordEmail :: f Boolean

  -- | Indicates to FusionAuth that it should skip registration 
  -- | verification even if it is enabled for the Application. 
  , skipRegistrationVerification :: f Boolean

  -- | Whether or not email verification should be skipped or not. 
  -- | In some cases, you might want to verify User’s emails and 
  -- | in other cases you won’t. This flag controls that behavior. 
  , skipVerification :: f Boolean

  -- | The User’s preferred delivery for verification codes
  -- | during a two factor login request. 
  , usersTwoFactorDelivery :: f TwoFactorDelivery

  , user :: UserOut

  , registration :: RegistrationOut
  
  | r)

newtype RegisterRequest = RegisterRequest {| RegisterRequestRep Maybe () }

derive instance newtypeRegisterRequest :: Newtype RegisterRequest _

instance encodeJsonRegisterRequest :: EncodeJson RegisterRequest where
  encodeJson (RegisterRequest r) 
      = "user" := r.user
     ~> "registration" := r.registration
     ~> "generateAuthenticationToken" :=? r.generateAuthenticationToken
    ~>? "sendSetPasswordEmail" :=? r.sendSetPasswordEmail
    ~>? "skipRegistrationVerification" :=? r.skipRegistrationVerification
    ~>? "skipVerification" :=? r.skipVerification
    ~>? "usersTwoFactorDelivery" :=? r.usersTwoFactorDelivery
    ~>? jsonEmptyObject


newtype RegisterResponse 
  = RegisterResponse
  { user :: UserIn
  , registration :: RegistrationIn
  }

derive instance newtypeRegisterResponse :: Newtype RegisterResponse _
derive newtype instance decodeRegisterResponse :: DecodeJson RegisterResponse

defaultRegisterRequest :: UserOut -> RegistrationOut -> RegisterRequest
defaultRegisterRequest user registration =
  RegisterRequest
  { user
  , registration
  , generateAuthenticationToken: Nothing
  , sendSetPasswordEmail: Nothing
  , skipRegistrationVerification: Nothing
  , skipVerification: Nothing
  , usersTwoFactorDelivery: Nothing
  }
