module FusionAuth.Username 
  ( Username
  , mkUsername
  , unUsername
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Char.Unicode (isAlphaNum)
import Data.Foldable (all)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty.CodeUnits (toCharArray)
import FusionAuth.Name (Name, mkName, unName)


newtype Username = Username Name

derive instance genericUsername :: Generic Username _
derive newtype instance eqUsername :: Eq Username
instance showUsername :: Show Username where show = genericShow
derive newtype instance encodeJsonUsername :: EncodeJson Username 
derive newtype instance decodeJsonUsername :: DecodeJson Username

mkUsername :: NonEmptyString -> Maybe Username
mkUsername nes 
  | name <- mkName nes
  , Just true <- all isAlphaNum <<< toCharArray <<< unName <$> name =
  Username <$> name
mkUsername _ = Nothing

unUsername :: Username -> NonEmptyString
unUsername (Username name) = unName name