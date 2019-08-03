module FusionAuth.Password 
  ( Password
  , mkPassword
  , unPassword
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Core as Json
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES


newtype Password = Password NonEmptyString

derive instance genericPassword :: Generic Password _
derive newtype instance eqPassword :: Eq Password
instance showPassword :: Show Password where show = genericShow
instance encodeJsonPassword :: EncodeJson Password where
  encodeJson (Password nes) = Json.fromString (NES.toString nes)
instance decodeJsonPassword :: DecodeJson Password where
  decodeJson json = Password 
    <$> note "Invalid Password" (Json.toString >=> NES.fromString $ json)

mkPassword :: NonEmptyString -> Maybe Password
mkPassword = pure <<< Password

unPassword :: Password -> NonEmptyString
unPassword (Password password) = password