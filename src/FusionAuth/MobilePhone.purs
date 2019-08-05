module FusionAuth.MobilePhone 
  ( MobilePhone
  , mkMobilePhone
  , printMobilePhone
  , unsafeMobilePhone
  ) where


import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Core as Json
import Data.Either (note)
import Data.Foldable (all)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Data.Set (Set, fromFoldable, member)
import Data.String.CodeUnits (toCharArray)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Partial.Unsafe (unsafePartial)


newtype MobilePhone = MobilePhone NonEmptyString

derive instance genericMobilePhone :: Generic MobilePhone _
derive newtype instance eqMobilePhone :: Eq MobilePhone
instance showMobilePhone :: Show MobilePhone where show = genericShow
instance encodeJsonMobilePhone :: EncodeJson MobilePhone where
  encodeJson (MobilePhone nes) = Json.fromString (NES.toString nes)
instance decodeJsonMobilePhone :: DecodeJson MobilePhone where
  decodeJson json = MobilePhone 
    <$> note "Invalid phone number" (Json.toString >=> NES.fromString $ json)

phoneChars :: Set Char
phoneChars = fromFoldable 
  ['-', '+', '(', ')', ' ', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

mkMobilePhone :: NonEmptyString -> Maybe MobilePhone
mkMobilePhone phone 
  | all (flip member phoneChars) (toCharArray $ NES.toString phone) = 
  Just $ MobilePhone phone
mkMobilePhone _ = Nothing

printMobilePhone :: MobilePhone -> NonEmptyString
printMobilePhone (MobilePhone phone) = phone

unsafeMobilePhone :: String -> MobilePhone
unsafeMobilePhone unsafe =
  unsafePartial $ fromJust $ mkMobilePhone <=< NES.fromString $ unsafe