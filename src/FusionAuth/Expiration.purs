module FusionAuth.Expiration 
  ( Expiration
  ) where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)


newtype Expiration = Expiration Instant

derive newtype instance eqExpiration :: Eq Expiration
derive newtype instance showExpiration :: Show Expiration
derive instance newtypeExpiration :: Newtype Expiration _
derive instance genericExpiration :: Generic Expiration _
instance encodeJsonExpiration :: EncodeJson Expiration where
  encodeJson = unwrap >>> unInstant >>> unwrap >>> encodeJson