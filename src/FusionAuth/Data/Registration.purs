module FusionAuth.Data.Registration 
  ( RegistrationOut (..)
  , RegistrationInRep
  , RegistrationIn (..)
  , RegistrationRep
  , defaultRegistration
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, jsonEmptyObject, (.:), (.:?), (:=), (:=?), (~>), (~>?))
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import FusionAuth.Data.ApplicationId (ApplicationId)
import FusionAuth.Data.IdentityProviderId (IdentityProviderId)
import FusionAuth.Data.Language (Language)
import FusionAuth.Data.RegistrationId (RegistrationId)
import FusionAuth.Data.Role (Role)
import FusionAuth.Data.Timezone (Timezone)
import FusionAuth.Data.Token (Token)
import FusionAuth.Data.UnixInstant (UnixInstant)
import FusionAuth.Data.Username (Username)


type RegistrationRep f r =
  -- | The Id of the Application that this registration is for. 
  ( applicationId :: ApplicationId

  -- | An object that can hold any information about the User 
  -- | for this registration that should be persisted. 
  , "data" :: f Json

  -- | Locale strings that give, in order, 
  -- | the User’s preferred languages for this registration. 
  , preferredLanguages :: f (Array Language)

  -- | The Id of this registration. If this is not specified, 
  -- | FusionAuth will create a random UUID. 
  , id :: f RegistrationId

  -- | The list of roles that the User has for this Application. 
  , roles :: f (Array Role)

  -- | The User’s preferred timezone for this Application registration.
  -- | example: 'America/Denver' or 'US/Mountain'
  , timezone :: f Timezone

  -- | The username of the User for this Application only. 
  , username :: f Username 

  | r)

newtype RegistrationOut = RegistrationOut {| RegistrationRep Maybe ()}

derive instance newtypeRegistrationOut :: Newtype RegistrationOut _

instance encodeJsonRegistrationOut :: EncodeJson RegistrationOut where
  encodeJson (RegistrationOut r) 
      = "applicationId" := r.applicationId 
     ~> "data" :=? r.data
    ~>? "id" :=? r.id 
    ~>? "preferredLanguages" :=? r.preferredLanguages 
    ~>? "roles" :=? r.roles 
    ~>? "timezone" :=? r.timezone 
    ~>? "username" :=? r.username 
    ~>? jsonEmptyObject

type RegistrationInRep = 
  ( authenticationToken :: Maybe Token

  -- | The instant that this registration was created. 
  , insertInstant :: UnixInstant
  
  -- | The instant that the User last logged 
  -- | into the Application for this registration. 
  , lastLoginInstant :: Maybe UnixInstant

  -- | Tokens returned from identity providers. 
  , tokens :: Maybe (Map IdentityProviderId Token)

  -- | Indicates if this User’s registration has been verified.  
  , verified :: Boolean
  )

newtype RegistrationIn 
  = RegistrationIn {| RegistrationRep Maybe RegistrationInRep}

derive instance newtypeRegistrationIn :: Newtype RegistrationIn _

instance decodeJsonRegistrationIn :: DecodeJson RegistrationIn where
  decodeJson json = do
    x <- decodeJson json
    applicationId <- x .: "applicationId"
    authenticationToken <- x .:? "authenticationToken"
    tokens <- x .:? "tokens"
    insertInstant <- x .: "insertInstant"
    lastLoginInstant <- x .:? "lastLoginInstant"
    metadata <- x .:? "data"
    id <- x .:? "id"
    preferredLanguages <- x .:? "preferredLanguages"
    roles <- x .:? "roles"
    timezone <- x .:? "timezone"
    username <- x .:? "username"
    verified <- x .: "verified"
    pure $ RegistrationIn
      { applicationId
      , authenticationToken
      , tokens
      , "data": metadata
      , id
      , insertInstant
      , lastLoginInstant
      , preferredLanguages
      , roles
      , timezone
      , username
      , verified
      }

defaultRegistration :: ApplicationId -> RegistrationOut
defaultRegistration applicationId =
  RegistrationOut
  { applicationId
  , "data": Nothing
  , id: Nothing
  , preferredLanguages: Nothing
  , roles: Nothing
  , timezone: Nothing
  , username: Nothing
  }

