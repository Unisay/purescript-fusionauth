module FusionAuth.Registration 
  ( RegistrationRequest (..)
  , defaultRequest
  , RegistrationResponse (..)
  , User
  , defaultUser
  , Registration
  , defaultRegistration
  ) where

import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe(..))
import FusionAuth.ApplicationId (ApplicationId)
import FusionAuth.Email (Email)
import FusionAuth.EncryptionScheme (EncryptionScheme)
import FusionAuth.Expiration (Expiration)
import FusionAuth.FirstName (FirstName)
import FusionAuth.FullName (FullName)
import FusionAuth.ImageUrl (ImageUrl)
import FusionAuth.Iso8601Date (Iso8601Date)
import FusionAuth.Language (Language)
import FusionAuth.LastName (LastName)
import FusionAuth.MiddleName (MiddleName)
import FusionAuth.MobilePhone (MobilePhone)
import FusionAuth.Password (Password)
import FusionAuth.RegistrationId (RegistrationId)
import FusionAuth.Role (Role)
import FusionAuth.Secret (Secret)
import FusionAuth.Timezone (Timezone)
import FusionAuth.TwoFactorDelivery (TwoFactorDelivery)
import FusionAuth.UserId (UserId)
import FusionAuth.Username (Username)


type Registration =
  -- | The Id of the Application that this registration is for. 
  { applicationId :: ApplicationId

  -- | An object that can hold any information about the User 
  -- | for this registration that should be persisted. 
  , "data" :: Maybe Json

  -- | Locale strings that give, in order, 
  -- | the User’s preferred languages for this registration. 
  , preferredLanguages :: Maybe (Array Language)

  -- | The Id of this registration. If this is not specified, 
  -- | FusionAuth will create a random UUID. 
  , id :: Maybe RegistrationId

  -- | The list of roles that the User has for this Application. 
  , roles :: Maybe (Array Role)

  -- | The User’s preferred timezone for this Application registration.
  -- | example: 'America/Denver' or 'US/Mountain'
  , timezone :: Maybe Timezone

  -- | The username of the User for this Application only. 
  , username :: Maybe Username 

  }

type User =
  { birthDate :: Maybe Iso8601Date

    -- | The Id to use for the new User. 
    -- | If absent, FusionAuth generates a random one. 
  , id :: Maybe UserId

  -- | An object that can hold any information about the User 
  -- | that should be persisted. 
  , "data" :: Maybe Json

  -- | The User’s email address. 
  -- | An email address is a unique in FusionAuth and stored in lower case. 
  , email :: Maybe Email

  -- | The method for encrypting the User’s password. 
  , encryptionScheme :: Maybe EncryptionScheme

  -- | The expiration instant of the User’s account. 
  -- | An expired user is not permitted to login. 
  , expiry :: Maybe Expiration

  -- | The actual use of this value is up to the 
  -- | `PasswordEncryptor` implementation. 
  , factor :: Maybe String

  -- | The first name of the User. 
  , firstName :: Maybe FirstName

  -- | The User’s last name. 
  , lastName :: Maybe LastName

  -- | The User’s middle name. 
  , middleName :: Maybe MiddleName

  -- | The User’s full name as a separate field
  -- | that is not calculated from firstName and lastName. 
  , fullName :: Maybe FullName

  -- | The URL that points to an image file that is the User’s profile image. 
  , imageUrl :: Maybe ImageUrl

  -- | The User’s mobile phone number. 
  , mobilePhone :: Maybe MobilePhone
  
  -- | The User’s plain texts password. 
  -- | This password will be hashed and the provided value 
  -- | will never be stored and cannot be retrieved. 
  , password :: Maybe Password

  -- | Indicates that the User’s password needs to be changed 
  -- | during their next login attempt. 
  , passwordChangeRequired :: Maybe Boolean

  -- | Locale strings that give, in order, 
  -- | the User’s preferred languages. 
  , preferredLanguages :: Maybe (Array Language)

  -- | The User’s preferred timezone. 
  -- | The string must be in an IANA time zone format. 
  , timezone :: Maybe Timezone

  -- | Determines if the User has two factor authentication 
  -- | enabled for their account or not. 
  , twoFactorEnabled :: Maybe Boolean

  -- | The Base64 encoded secret used to generate Two Factor verification codes. 
  , twoFactorSecret :: Maybe Secret

  -- | The username of the User. 
  , username :: Maybe Username
  }

type RegistrationRequest =
  -- | Determines if FusionAuth should generate an 
  -- | Authentication Token for this registration. 
  { generateAuthenticationToken :: Maybe Boolean

  -- | Whether or not the User is sent an email asking them 
  -- | to setup their password. This is useful if someone else 
  -- | is creating an account for a User with an Application. 
  , sendSetPasswordEmail :: Maybe Boolean

  -- | Indicates to FusionAuth that it should skip registration 
  -- | verification even if it is enabled for the Application. 
  , skipRegistrationVerification :: Maybe Boolean

  -- | Whether or not email verification should be skipped or not. 
  -- | In some cases, you might want to verify User’s emails and 
  -- | in other cases you won’t. This flag controls that behavior. 
  , skipVerification :: Maybe Boolean

  -- | The User’s preferred delivery for verification codes
  -- | during a two factor login request. 
  , usersTwoFactorDelivery :: Maybe TwoFactorDelivery

  , user :: User

  , registration :: Registration
  
  }

defaultRequest :: User -> Registration -> RegistrationRequest
defaultRequest user registration =
  { user
  , registration
  , generateAuthenticationToken: Nothing
  , sendSetPasswordEmail: Nothing
  , skipRegistrationVerification: Nothing
  , skipVerification: Nothing
  , usersTwoFactorDelivery: Nothing
  }

defaultUser :: User
defaultUser =
  { birthDate: Nothing
  , "data": Nothing
  , email: Nothing
  , encryptionScheme: Nothing
  , expiry: Nothing
  , factor: Nothing
  , firstName: Nothing
  , fullName: Nothing
  , id: Nothing
  , imageUrl: Nothing
  , lastName: Nothing
  , middleName: Nothing
  , mobilePhone: Nothing
  , password: Nothing
  , passwordChangeRequired: Nothing
  , preferredLanguages: Nothing
  , timezone: Nothing
  , twoFactorEnabled: Nothing
  , twoFactorSecret: Nothing
  , username: Nothing
  }

defaultRegistration :: ApplicationId -> Registration
defaultRegistration applicationId =
  { applicationId
  , "data": Nothing
  , id: Nothing
  , preferredLanguages: Nothing
  , roles: Nothing
  , timezone: Nothing
  , username: Nothing
  }

type RegistrationResponse = 
  {}