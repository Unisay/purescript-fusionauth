module FusionAuth.IdentityProviderId 
  ( IdentityProviderId (..)
  ) where

import Prelude

import Data.Argonaut (class DecodeJson)
import Data.Newtype (class Newtype)
import FusionAuth.Name (Name)


newtype IdentityProviderId = IdentityProviderId Name

derive newtype instance eqIdentityProviderId :: Eq IdentityProviderId
derive newtype instance ordIdentityProviderId :: Ord IdentityProviderId
derive newtype instance showIdentityProviderId :: Show IdentityProviderId
derive newtype instance decodeJsonIdentityProviderId :: DecodeJson IdentityProviderId
derive instance newtypeIdentityProviderId :: Newtype IdentityProviderId _
