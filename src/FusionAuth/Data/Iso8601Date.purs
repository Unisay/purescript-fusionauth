module FusionAuth.Data.Iso8601Date 
  ( Iso8601Date (..)
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut as Json
import Data.Date (Date, exactDate)
import Data.DateTime (DateTime(..))
import Data.Either (either, note)
import Data.Enum (class BoundedEnum, toEnum)
import Data.Foldable (foldMap)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format)
import Data.Int (fromString)
import Data.List (List(..), (:))
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits (singleton) as S
import Text.Parsing.StringParser (Parser, fail, runParser)
import Text.Parsing.StringParser.CodeUnits (anyDigit, char)
import Text.Parsing.StringParser.Combinators (many)

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

instance decodeJsonIso8601Date :: DecodeJson Iso8601Date where
  decodeJson json = do
    raw <- note "ISO 8601 Date must be represented by JSON string" 
      $ Json.toString json
    date <- either (show >>> throwError) pure $ runParser dateP raw
    pure $ Iso8601Date date

dateP :: Parser Date
dateP = do
  yyyy <- enumP "Invalid Year" 
  _ <- char '-'
  mm <- enumP "Invalid Month"
  _ <- char '-'
  dd <- enumP "Invalid Day"
  maybe (fail "Invalid Date") pure $ exactDate yyyy mm dd

enumP :: forall e. BoundedEnum e => String -> Parser e
enumP err = intP <#> toEnum >>= maybe (fail err) pure

intP :: Parser Int
intP = many anyDigit 
  >>= foldMap S.singleton 
  >>> fromString 
  >>> maybe (fail "Invalid Int") pure
