module FusionAuth.ApiUrl 
  ( ApiUrl (..)
  , unApiUrl
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Core as Json
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES

newtype ApiUrl = ApiUrl NonEmptyString

derive instance genericApiUrl :: Generic ApiUrl _
derive instance newtypeApiUrl :: Newtype ApiUrl _
instance showApiUrl :: Show ApiUrl where show = genericShow
instance encodeJsonApiUrl :: EncodeJson ApiUrl where
  encodeJson (ApiUrl nes) = Json.fromString (NES.toString nes)
instance decodeJsonApiUrl :: DecodeJson ApiUrl where
  decodeJson json =
    ApiUrl <$> note "Empty API URL" (Json.toString >=> NES.fromString $ json)

unApiUrl :: ApiUrl -> String
unApiUrl (ApiUrl nes) = NES.toString nes