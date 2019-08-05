module FusionAuth.MiddleName 
  ( MiddleName
  , mkMiddleName
  , printMiddleName
  , unsafeMiddleName
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.String.NonEmpty (NonEmptyString)
import FusionAuth.Name (Name, mkName, printName, unsafeName)


newtype MiddleName = MiddleName Name

derive instance genericMiddleName :: Generic MiddleName _
derive newtype instance eqMiddleName :: Eq MiddleName
instance showMiddleName :: Show MiddleName where show = genericShow
derive newtype instance encodeJsonMiddleName :: EncodeJson MiddleName 
derive newtype instance decodeJsonMiddleName :: DecodeJson MiddleName

mkMiddleName :: NonEmptyString -> Maybe MiddleName
mkMiddleName name = MiddleName <$> mkName name

printMiddleName :: MiddleName -> String
printMiddleName (MiddleName name) = printName name

unsafeMiddleName :: String -> MiddleName
unsafeMiddleName = MiddleName <<< unsafeName