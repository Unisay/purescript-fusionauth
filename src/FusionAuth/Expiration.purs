module FusionAuth.Expiration 
  ( Expiration
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import FusionAuth.UnixInstant (UnixInstant)


newtype Expiration = Expiration UnixInstant

derive newtype instance eqExpiration :: Eq Expiration
derive newtype instance showExpiration :: Show Expiration
derive instance newtypeExpiration :: Newtype Expiration _
derive instance genericExpiration :: Generic Expiration _
derive newtype instance encodeJsonExpiration :: EncodeJson Expiration
derive newtype instance decodeJsonExpiration :: DecodeJson Expiration