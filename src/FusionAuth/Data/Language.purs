module FusionAuth.Data.Language 
  ( Language
  , mkLanguage
  , printLanguage
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.String.NonEmpty.Internal (NonEmptyString)
import FusionAuth.Data.Name (Name, mkName, printName)


newtype Language = Language Name

derive instance genericLanguage :: Generic Language _
derive newtype instance eqLanguage :: Eq Language
instance showLanguage :: Show Language where show = genericShow
derive newtype instance encodeJsonLanguage :: EncodeJson Language 
derive newtype instance decodeJsonLanguage :: DecodeJson Language

mkLanguage :: NonEmptyString -> Maybe Language
mkLanguage name = Language <$> mkName name

printLanguage :: Language -> String
printLanguage (Language name) = printName name