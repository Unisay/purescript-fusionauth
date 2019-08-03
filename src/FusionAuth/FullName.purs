module FusionAuth.FullName 
  ( FullName
  , mkFullName
  , unFullName
  , unsafeFullName
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.String.NonEmpty.Internal (NonEmptyString)
import FusionAuth.Name (Name, mkName, unName, unsafeName)


newtype FullName = FullName Name

derive instance genericFullName :: Generic FullName _
derive newtype instance eqFullName :: Eq FullName
instance showFullName :: Show FullName where show = genericShow
derive newtype instance encodeJsonFullName :: EncodeJson FullName 
derive newtype instance decodeJsonFullName :: DecodeJson FullName

mkFullName :: NonEmptyString -> Maybe FullName
mkFullName name = FullName <$> mkName name

unFullName :: FullName -> NonEmptyString
unFullName (FullName name) = unName name

unsafeFullName :: String -> FullName
unsafeFullName = FullName <<< unsafeName