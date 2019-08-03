module FusionAuth.Iso8601Date 
  ( Iso8601Date (..)
  ) where

import Prelude

import Data.Argonaut (class EncodeJson)
import Data.Argonaut as Json
import Data.Date (Date)
import Data.DateTime (DateTime(..))
import Data.Formatter.DateTime (FormatterCommand(..), Formatter, format)
import Data.List (List(..), (:))
import Data.Newtype (class Newtype, unwrap)

newtype Iso8601Date = Iso8601Date Date

derive instance newtypeIso8601Date :: Newtype Iso8601Date _
derive newtype instance showIso8601Date :: Show Iso8601Date
derive newtype instance eqIso8601Date :: Eq Iso8601Date

formatter :: Formatter
formatter = YearFull : dash : MonthTwoDigits : dash : DayOfMonthTwoDigits : Nil
  where dash = Placeholder "-"

instance encodeJsonIso8601Date :: EncodeJson Iso8601Date where
  encodeJson = unwrap 
    >>> flip DateTime bottom 
    >>> format formatter
    >>> Json.fromString
