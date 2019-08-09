module FusionAuth.Register
  ( RegisterRequestRep
  , RegisterRequest
  , RegisterResponse(..)
  , defaultRegisterRequest
  , encodeRegisterRequest
  , DuplicateField (..)
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, Json, decodeJson, jsonEmptyObject, (.:), (:=), (:=?), (~>), (~>?))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import FusionAuth.Data.Registration (RegistrationIn, RegistrationOut, decodeRegistrationIn, encodeRegistrationOut)
import FusionAuth.Data.TwoFactorDelivery (TwoFactorDelivery)
import FusionAuth.Data.User (User, UserOut, decodeUser, encodeUserOut)


data DuplicateField = UserName | UserEmail

derive instance eqDuplicateField :: Eq DuplicateField
derive instance genericDuplicateField :: Generic DuplicateField _
instance showDuplicateField :: Show DuplicateField where show = genericShow

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

type RegisterRequest = {| RegisterRequestRep Maybe () }

encodeRegisterRequest :: RegisterRequest -> Json
encodeRegisterRequest r
      = "user" := encodeUserOut r.user
     ~> "registration" := encodeRegistrationOut r.registration
     ~> "generateAuthenticationToken" :=? r.generateAuthenticationToken
    ~>? "sendSetPasswordEmail" :=? r.sendSetPasswordEmail
    ~>? "skipRegistrationVerification" :=? r.skipRegistrationVerification
    ~>? "skipVerification" :=? r.skipVerification
    ~>? "usersTwoFactorDelivery" :=? r.usersTwoFactorDelivery
    ~>? jsonEmptyObject


data RegisterResponse 
  = UserRegistered { user :: User, registration :: RegistrationIn }
  | NonUniqueUser (NonEmptyArray DuplicateField)

instance decodeJsonRegisterResponse :: DecodeJson RegisterResponse where
  decodeJson json = do
    x <- decodeJson json
    user <- x .: "user" >>= decodeUser
    registration <- x .: "registration" >>= decodeRegistrationIn
    pure $ UserRegistered { user, registration }

defaultRegisterRequest :: UserOut -> RegistrationOut -> RegisterRequest
defaultRegisterRequest user registration =
  { user
  , registration
  , generateAuthenticationToken: Nothing
  , sendSetPasswordEmail: Nothing
  , skipRegistrationVerification: Nothing
  , skipVerification: Nothing
  , usersTwoFactorDelivery: Nothing
  }
