module FusionAuth.Username 
  ( Username
  , mkUsername
  , printUsername
  , unsafeUsername
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Char.Unicode (isAlphaNum)
import Data.Foldable (all)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.String.CodeUnits (toCharArray)
import FusionAuth.Name (Name, mkName, printName)
import Partial.Unsafe (unsafePartial)


newtype Username = Username Name

derive instance genericUsername :: Generic Username _
derive newtype instance eqUsername :: Eq Username
instance showUsername :: Show Username where show = genericShow
derive newtype instance encodeJsonUsername :: EncodeJson Username 
derive newtype instance decodeJsonUsername :: DecodeJson Username

mkUsername :: NonEmptyString -> Maybe Username
mkUsername nes 
  | name <- mkName nes
  , Just true <- all isAlphaNum <<< toCharArray <<< printName <$> name =
  Username <$> name
mkUsername _ = Nothing

printUsername :: Username -> String
printUsername (Username name) = printName name

unsafeUsername :: String -> Username
unsafeUsername name = 
  unsafePartial $ fromJust $ mkUsername <=< NES.fromString $ name