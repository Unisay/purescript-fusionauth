module FusionAuth.Role 
  ( Role
  , mkRole
  , unRole
  , unsafeRole
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.String.NonEmpty (NonEmptyString)
import FusionAuth.Name (Name, mkName, unName, unsafeName)


newtype Role = Role Name

derive instance genericRole :: Generic Role _
derive newtype instance eqRole :: Eq Role
instance showRole :: Show Role where show = genericShow
derive newtype instance encodeJsonRole :: EncodeJson Role 
derive newtype instance decodeJsonRole :: DecodeJson Role

mkRole :: NonEmptyString -> Maybe Role
mkRole = map Role <<< mkName

unRole :: Role -> NonEmptyString
unRole (Role name) = unName name

unsafeRole :: String -> Role
unsafeRole = Role <<< unsafeName