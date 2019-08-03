module FusionAuth.ApiKey 
  ( ApiKey
  , mkApiKey
  , readApiKey
  , unApiKey
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

unApiKey :: ApiKey -> String
unApiKey (ApiKey key) = NES.toString key

unsafeApiKey :: String -> ApiKey
unsafeApiKey unsafe = ApiKey $ unsafePartial $ fromJust $ NES.fromString unsafe