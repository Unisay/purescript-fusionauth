module FusionAuth.Data.User 
  ( UserOutRep
  , UserOut
  , UserRep
  , User
  , defaultUserOut
  , encodeUserOut
  , decodeUser
  ) where

import Prelude

import Data.Argonaut (Json, decodeJson, jsonEmptyObject, (.:), (.:?), (:=?), (~>?))
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty)
import FusionAuth.Data.Email (Email)
import FusionAuth.Data.EncryptionScheme (EncryptionScheme)
import FusionAuth.Data.Expiration (Expiration)
import FusionAuth.Data.FirstName (FirstName)
import FusionAuth.Data.FullName (FullName)
import FusionAuth.Data.ImageUrl (ImageUrl)
import FusionAuth.Data.Iso8601Date (Iso8601Date)
import FusionAuth.Data.Language (Language)
import FusionAuth.Data.LastName (LastName)
import FusionAuth.Data.MiddleName (MiddleName)
import FusionAuth.Data.MobilePhone (MobilePhone)
import FusionAuth.Data.Password (Password)
import FusionAuth.Data.Registration (RegistrationIn, RegistrationIn')
import FusionAuth.Data.Secret (Secret)
import FusionAuth.Data.Timezone (Timezone)
import FusionAuth.Data.UnixInstant (UnixInstant)
import FusionAuth.Data.UserId (UserId)
import FusionAuth.Data.Username (Username)



type UserOutRep f r = 
  ( birthDate :: f Iso8601Date

    -- | The Id to use for the new User. 
    -- | If absent, FusionAuth generates a random one. 
  , id :: f UserId

  -- | An object that can hold any information about the User 
  -- | that should be persisted. 
  , "data" :: f Json

  -- | The User’s email address. 
  -- | An email address is a unique in FusionAuth and stored in lower case. 
  , email :: f Email

  -- | The method for encrypting the User’s password. 
  , encryptionScheme :: f EncryptionScheme

  -- | The expiration instant of the User’s account. 
  -- | An expired user is not permitted to login. 
  , expiry :: f Expiration

  -- | The actual use of this value is up to the 
  -- | `PasswordEncryptor` implementation. 
  , factor :: f String

  -- | The first name of the User. 
  , firstName :: f FirstName

  -- | The User’s last name. 
  , lastName :: f LastName

  -- | The User’s middle name. 
  , middleName :: f MiddleName

  -- | The User’s full name as a separate optionalField
  -- | that is not calculated from firstName and lastName. 
  , fullName :: f FullName

  -- | The URL that points to an image file that is the User’s profile image. 
  , imageUrl :: f ImageUrl

  -- | The User’s mobile phone number. 
  , mobilePhone :: f MobilePhone
  
  -- | The User’s plain texts password. 
  -- | This password will be hashed and the provided value 
  -- | will never be stored and cannot be retrieved. 
  , password :: f Password

  -- | Indicates that the User’s password needs to be changed 
  -- | during their next login attempt. 
  , passwordChangeRequired :: f Boolean

  -- | Locale strings that give, in order, 
  -- | the User’s preferred languages. 
  , preferredLanguages :: f (Array Language)

  -- | The User’s preferred timezone. 
  -- | The string must be in an IANA time zone format. 
  , timezone :: f Timezone

  -- | Determines if the User has two factor authentication 
  -- | enabled for their account or not. 
  , twoFactorEnabled :: f Boolean

  -- | The Base64 encoded secret used to generate Two Factor verification codes. 
  , twoFactorSecret :: f Secret

  -- | The username of the User. 
  , username :: f Username
  
  | r)

type UserOut = {| UserOutRep Maybe ()}

encodeUserOut :: UserOut -> Json
encodeUserOut u
      = "birthDate" :=? u.birthDate
    ~>? "data" :=? u.data
    ~>? "email" :=? u.email
    ~>? "encryptionScheme" :=? u.encryptionScheme
    ~>? "expiry" :=? u.expiry
    ~>? "factor" :=? u.factor
    ~>? "firstName" :=? u.firstName
    ~>? "fullName" :=? u.fullName
    ~>? "id" :=? u.id
    ~>? "imageUrl" :=? u.imageUrl
    ~>? "lastName" :=? u.lastName
    ~>? "middleName" :=? u.middleName
    ~>? "mobilePhone" :=? u.mobilePhone
    ~>? "password" :=? u.password
    ~>? "passwordChangeRequired" :=? u.passwordChangeRequired
    ~>? "preferredLanguages" :=? u.preferredLanguages
    ~>? "timezone" :=? u.timezone
    ~>? "twoFactorEnabled" :=? u.twoFactorEnabled
    ~>? "twoFactorSecret" :=? u.twoFactorSecret
    ~>? "username" :=? u.username
    ~>?  jsonEmptyObject

type UserRep = 
  ( active :: Boolean 
  , insertInstant :: UnixInstant
  , lastLoginInstant :: Maybe UnixInstant
  , passwordLastUpdateInstant :: UnixInstant
  , registrations :: Maybe (NonEmpty Array RegistrationIn)
  )

type User = {| UserOutRep Maybe UserRep}

decodeUser :: Json -> Either String User
decodeUser json = do
    x <- decodeJson json
    active <- x .: "active"
    birthDate <- x .:? "birthDate"
    metadata <- x .:? "data"
    email <- x .:? "email"
    encryptionScheme <- x .:? "encryptionScheme"
    expiry <- x .:? "expiry"
    factor <- x .:? "factor"
    firstName <- x .:? "firstName"
    fullName <- x .:? "fullName"
    id <- x .:? "id"
    imageUrl <- x .:? "imageUrl"
    lastName <- x .:? "lastName"
    middleName <- x .:? "middleName"
    mobilePhone <- x .:? "mobilePhone"
    password <- x .:? "password"
    passwordChangeRequired <- x .:? "passwordChangeRequired"
    preferredLanguages <- x .:? "preferredLanguages"
    timezone <- x .:? "timezone"
    twoFactorEnabled <- x .:? "twoFactorEnabled"
    twoFactorSecret <- x .:? "twoFactorSecret"
    username <- x .:? "username"
    insertInstant <- x .: "insertInstant"
    lastLoginInstant <- x .:? "insertInstant"
    passwordLastUpdateInstant <- x .: "passwordLastUpdateInstant"
    registrations  <- x .:? "registrations"
      # (map >>> map >>> map) (unwrap :: RegistrationIn' -> RegistrationIn)
    
    pure
      { active
      , birthDate
      , "data": metadata
      , email
      , encryptionScheme
      , expiry
      , factor
      , firstName
      , fullName
      , id
      , imageUrl
      , lastName
      , middleName
      , mobilePhone
      , password
      , passwordChangeRequired
      , preferredLanguages
      , timezone
      , twoFactorEnabled
      , twoFactorSecret
      , username
      , insertInstant
      , lastLoginInstant
      , passwordLastUpdateInstant
      , registrations
      }

defaultUserOut :: UserOut
defaultUserOut =
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

