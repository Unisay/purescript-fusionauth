module FusionAuth.Data.Role 
  ( Role
  , mkRole
  , printRole
  , unsafeRole
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.String.NonEmpty (NonEmptyString)
import FusionAuth.Data.Name (Name, mkName, printName, unsafeName)


newtype Role = Role Name

derive instance genericRole :: Generic Role _
derive newtype instance eqRole :: Eq Role
instance showRole :: Show Role where show = genericShow
derive newtype instance encodeJsonRole :: EncodeJson Role 
derive newtype instance decodeJsonRole :: DecodeJson Role

mkRole :: NonEmptyString -> Maybe Role
mkRole = map Role <<< mkName

printRole :: Role -> String
printRole (Role name) = printName name

unsafeRole :: String -> Role
unsafeRole = Role <<< unsafeName