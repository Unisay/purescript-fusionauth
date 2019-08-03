module FusionAuth.Timezone 
  ( Timezone
  , mkTimezone
  , unTimezone
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.String.NonEmpty.Internal (NonEmptyString)
import FusionAuth.Name (Name, mkName, unName)


newtype Timezone = Timezone Name

derive instance genericTimezone :: Generic Timezone _
derive newtype instance eqTimezone :: Eq Timezone
instance showTimezone :: Show Timezone where show = genericShow
derive newtype instance encodeJsonTimezone :: EncodeJson Timezone 
derive newtype instance decodeJsonTimezone :: DecodeJson Timezone

mkTimezone :: NonEmptyString -> Maybe Timezone
mkTimezone name = Timezone <$> mkName name

unTimezone :: Timezone -> NonEmptyString
unTimezone (Timezone name) = unName name