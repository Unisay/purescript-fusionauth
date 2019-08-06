module FusionAuth.Data.UnixInstant 
  ( UnixInstant (..)
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, encodeJson)
import Data.Argonaut as Json
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (note)
import Data.Newtype (class Newtype, wrap, unwrap)


newtype UnixInstant = UnixInstant Instant

derive newtype instance eqUnixInstant :: Eq UnixInstant
derive newtype instance showUnixInstant :: Show UnixInstant
derive instance newtypeUnixInstant :: Newtype UnixInstant _

instance encodeJsonUnixInstant :: EncodeJson UnixInstant where
  encodeJson = unwrap >>> unInstant >>> unwrap >>> encodeJson

instance decodeJsonUnixInstant :: DecodeJson UnixInstant where
  decodeJson = Json.toNumber 
    >>> note "Instant is not a number" 
    >>> map wrap >=> instant >>> note "Invalid Instant" 
    >>> map UnixInstant