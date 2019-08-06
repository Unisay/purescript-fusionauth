module FusionAuth.Data.Password 
  ( Password
  , mkPassword
  , printPassword
  , unsafePassword
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Core as Json
import Data.Either (note)
import Data.Maybe (Maybe, fromJust)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Partial.Unsafe (unsafePartial)


newtype Password = Password NonEmptyString

derive newtype instance eqPassword :: Eq Password
instance showPassword :: Show Password where show = const "Password(masked)"
instance encodeJsonPassword :: EncodeJson Password where
  encodeJson (Password nes) = Json.fromString (NES.toString nes)
instance decodeJsonPassword :: DecodeJson Password where
  decodeJson json = Password 
    <$> note "Invalid Password" (Json.toString >=> NES.fromString $ json)

mkPassword :: NonEmptyString -> Maybe Password
mkPassword = pure <<< Password

printPassword :: Password -> NonEmptyString
printPassword (Password password) = password

unsafePassword :: String -> Password
unsafePassword unsafe = 
  unsafePartial $ fromJust $ mkPassword <=< NES.fromString $ unsafe