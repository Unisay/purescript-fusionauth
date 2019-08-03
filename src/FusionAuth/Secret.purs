module FusionAuth.Secret 
  ( Secret
  , mkSecret
  , unSecret
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Core as Json
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES


newtype Secret = Secret NonEmptyString

derive instance genericSecret :: Generic Secret _
derive newtype instance showSecret :: Show Secret
derive newtype instance eqSecret :: Eq Secret
instance encodeJsonSecret :: EncodeJson Secret where
  encodeJson (Secret nes) = Json.fromString (NES.toString nes)
instance decodeJsonSecret :: DecodeJson Secret where
  decodeJson json =
    Secret <$> note "Empty secret" (Json.toString >=> NES.fromString $ json)

mkSecret :: NonEmptyString -> Maybe Secret
mkSecret = pure <<< Secret

unSecret :: Secret -> NonEmptyString
unSecret (Secret nes) = nes