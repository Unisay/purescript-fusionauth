module FusionAuth.Data.LastName 
  ( LastName
  , mkLastName
  , printLastName
  , unsafeLastName
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.String.NonEmpty (NonEmptyString)
import FusionAuth.Data.Name (Name, mkName, printName, unsafeName)


newtype LastName = LastName Name

derive instance genericLastName :: Generic LastName _
derive newtype instance eqLastName :: Eq LastName
instance showLastName :: Show LastName where show = genericShow
derive newtype instance encodeJsonLastName :: EncodeJson LastName 
derive newtype instance decodeJsonLastName :: DecodeJson LastName

mkLastName :: NonEmptyString -> Maybe LastName
mkLastName name = LastName <$> mkName name

printLastName :: LastName -> String
printLastName (LastName name) = printName name

unsafeLastName :: String -> LastName
unsafeLastName = LastName <<< unsafeName