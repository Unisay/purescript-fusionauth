module FusionAuth.LastName 
  ( LastName
  , mkLastName
  , unLastName
  , unsafeLastName
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.String.NonEmpty.Internal (NonEmptyString)
import FusionAuth.Name (Name, mkName, unName, unsafeName)


newtype LastName = LastName Name

derive instance genericLastName :: Generic LastName _
derive newtype instance eqLastName :: Eq LastName
instance showLastName :: Show LastName where show = genericShow
derive newtype instance encodeJsonLastName :: EncodeJson LastName 
derive newtype instance decodeJsonLastName :: DecodeJson LastName

mkLastName :: NonEmptyString -> Maybe LastName
mkLastName name = LastName <$> mkName name

unLastName :: LastName -> NonEmptyString
unLastName (LastName name) = unName name

unsafeLastName :: NonEmptyString -> LastName
unsafeLastName = LastName <<< unsafeName