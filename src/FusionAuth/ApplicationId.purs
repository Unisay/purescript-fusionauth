module FusionAuth.ApplicationId 
  ( ApplicationId (..)
  , mkApplicationId
  ) where

import Prelude

import Data.Argonaut (class EncodeJson)
import Data.Argonaut as Json
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.String.NonEmpty (NonEmptyString, toString)
import Data.UUID (UUID, parseUUID)
import Data.UUID as UUID


newtype ApplicationId = ApplicationId UUID

derive instance newtypeApplicationId :: Newtype ApplicationId _
derive instance genericApplicationId :: Generic ApplicationId _
derive newtype instance eqApplicationId :: Eq ApplicationId
instance showApplicationId :: Show ApplicationId where show = genericShow
instance encodeJsonApplicationId :: EncodeJson ApplicationId where
  encodeJson (ApplicationId uuid) = Json.fromString (UUID.toString uuid)

mkApplicationId :: NonEmptyString -> Maybe ApplicationId
mkApplicationId = toString >>> parseUUID >>> map ApplicationId