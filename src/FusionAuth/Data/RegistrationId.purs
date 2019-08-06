module FusionAuth.Data.RegistrationId 
  ( RegistrationId (..)
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut as Json
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Data.UUID as UUID


newtype RegistrationId = RegistrationId UUID

derive instance newtypeRegistrationId :: Newtype RegistrationId _
derive instance genericRegistrationId :: Generic RegistrationId _
derive newtype instance eqRegistrationId :: Eq RegistrationId
instance showRegistrationId :: Show RegistrationId where show = genericShow
instance encodeJsonRegistrationId :: EncodeJson RegistrationId where
  encodeJson (RegistrationId uuid) = Json.fromString (UUID.toString uuid)
instance decodeJsonRegistrationId :: DecodeJson RegistrationId where
  decodeJson json = 
    RegistrationId <$> note "Failed to decode RegistrationId" mbUuid
    where mbUuid = Json.toString json >>= UUID.parseUUID

