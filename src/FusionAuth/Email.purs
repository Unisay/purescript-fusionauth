module FusionAuth.Email 
  ( Email
  , mkEmail
  , unEmail
  , unsafeEmail
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Core as Json
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString, Pattern(..), contains)
import Data.String.NonEmpty as NES


newtype Email = Email NonEmptyString

derive instance genericEmail :: Generic Email _
derive newtype instance eqEmail :: Eq Email
instance showEmail :: Show Email where show = genericShow
instance encodeJsonEmail :: EncodeJson Email where
  encodeJson (Email nes) = Json.fromString (NES.toString nes)
instance decodeJsonEmail :: DecodeJson Email where
  decodeJson json = Email 
    <$> note "Invalid Email" (Json.toString >=> NES.fromString $ json)

mkEmail :: NonEmptyString -> Maybe Email
mkEmail email | contains (Pattern "@") email = Just $ Email email
mkEmail _ = Nothing

unEmail :: Email -> NonEmptyString
unEmail (Email email) = email

unsafeEmail :: NonEmptyString -> Email
unsafeEmail = Email