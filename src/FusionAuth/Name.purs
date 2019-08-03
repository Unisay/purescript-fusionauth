module FusionAuth.Name 
  ( Name   
  , mkName
  , unName 
  , unsafeName
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Core as Json
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.String.NonEmpty (NonEmptyString, trim)
import Data.String.NonEmpty as NES

newtype Name = Name NonEmptyString
derive instance genericName :: Generic Name _
derive newtype instance eqName :: Eq Name
instance showName :: Show Name where show = genericShow
instance encodeJsonName :: EncodeJson Name where
  encodeJson (Name nes) = Json.fromString (NES.toString nes)
instance decodeJsonName :: DecodeJson Name where
  decodeJson json =
    Name <$> note "Empty string" (Json.toString >=> NES.fromString $ json)

mkName :: NonEmptyString -> Maybe Name
mkName nes = Name <$> trim nes

unName :: Name -> NonEmptyString
unName (Name name) = name

unsafeName :: NonEmptyString -> Name
unsafeName = Name