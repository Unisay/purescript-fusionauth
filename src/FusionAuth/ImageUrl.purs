module FusionAuth.ImageUrl 
  ( ImageUrl
  ) where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES

newtype ImageUrl = ImageUrl NonEmptyString

derive newtype instance eqImageUrl :: Eq ImageUrl
derive newtype instance showImageUrl :: Show ImageUrl
derive instance genericImageUrl :: Generic ImageUrl _
derive instance newtypeImageUrl :: Newtype ImageUrl _
instance encodeJsonImageUrl :: EncodeJson ImageUrl where
  encodeJson = unwrap >>> NES.toString >>> encodeJson