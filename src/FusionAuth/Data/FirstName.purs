module FusionAuth.Data.FirstName 
  ( FirstName
  , mkFirstName
  , printFirstName
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.String.NonEmpty.Internal (NonEmptyString)
import FusionAuth.Data.Name (Name, mkName, printName, unsafeName)


newtype FirstName = FirstName Name

derive instance genericFirstName :: Generic FirstName _
derive newtype instance eqFirstName :: Eq FirstName
instance showFirstName :: Show FirstName where show = genericShow
derive newtype instance encodeJsonFirstName :: EncodeJson FirstName 
derive newtype instance decodeJsonFirstName :: DecodeJson FirstName

mkFirstName :: NonEmptyString -> Maybe FirstName
mkFirstName name = FirstName <$> mkName name

printFirstName :: FirstName -> String
printFirstName (FirstName name) = printName name

unsafeFirstName :: String -> FirstName
unsafeFirstName = FirstName <<< unsafeName