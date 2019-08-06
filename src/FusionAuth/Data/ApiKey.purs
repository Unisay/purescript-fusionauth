module FusionAuth.Data.ApiKey 
  ( ApiKey
  , mkApiKey
  , readApiKey
  , printApiKey
  , unsafeApiKey
  ) where

import Prelude

import Data.Maybe (Maybe, fromJust)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Partial.Unsafe (unsafePartial)

newtype ApiKey = ApiKey NonEmptyString

derive instance eqApiKey :: Eq ApiKey

mkApiKey :: NonEmptyString -> ApiKey
mkApiKey = ApiKey

readApiKey :: String -> Maybe ApiKey
readApiKey = map ApiKey <<< NES.fromString

printApiKey :: ApiKey -> String
printApiKey (ApiKey key) = NES.toString key

unsafeApiKey :: String -> ApiKey
unsafeApiKey unsafe = ApiKey $ unsafePartial $ fromJust $ NES.fromString unsafe