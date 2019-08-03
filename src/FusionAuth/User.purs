module FusionAuth.User 
  ( UserRep
  , UserOut (..)
  , UserInRep
  , UserIn (..)
  , defaultUserOut
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, jsonEmptyObject, (.:), (.:?), (:=?), (~>?))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty)
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
import FusionAuth.Registration (RegistrationIn)
import FusionAuth.Secret (Secret)
import FusionAuth.Timezone (Timezone)
import FusionAuth.UnixInstant (UnixInstant)
import FusionAuth.UserId (UserId)
import FusionAuth.Username (Username)



type UserRep f r = 
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

newtype UserOut = UserOut {| UserRep Maybe ()}

derive instance newtypeUserOut :: Newtype UserOut _

instance encodeJsonUserOut :: EncodeJson UserOut where 
  encodeJson (UserOut u) 
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

type UserInRep = 
  ( active :: Boolean 
  , insertInstant :: UnixInstant
  , lastLoginInstant :: Maybe UnixInstant
  , passwordLastUpdateInstant :: UnixInstant
  , registrations :: Maybe (NonEmpty Array RegistrationIn)
  )

newtype UserIn = UserIn {| UserRep Maybe UserInRep}

derive instance newtypeUserIn :: Newtype UserIn _

instance decodeJsonUserIn :: DecodeJson UserIn where
  decodeJson json = do
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
    registrations <- x .:? "registrations"
    pure $ UserIn
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
defaultUserOut = UserOut
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

