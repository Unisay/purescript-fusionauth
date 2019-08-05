module FusionAuth.Token 
  ( Token
  , mkToken
  , printToken
  , unsafeToken
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Core as Json
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromJust)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Partial.Unsafe (unsafePartial)


newtype Token = Token NonEmptyString

derive instance genericToken :: Generic Token _
derive newtype instance eqToken :: Eq Token
instance showToken :: Show Token where show = const "Token(masked)"
instance encodeJsonToken :: EncodeJson Token where
  encodeJson (Token nes) = Json.fromString (NES.toString nes)
instance decodeJsonToken :: DecodeJson Token where
  decodeJson json = Token 
    <$> note "Invalid Token" (Json.toString >=> NES.fromString $ json)

mkToken :: NonEmptyString -> Maybe Token
mkToken = pure <<< Token

printToken :: Token -> String
printToken (Token password) = NES.toString password

unsafeToken :: String -> Token
unsafeToken unsafe = 
  unsafePartial $ fromJust $ mkToken <=< NES.fromString $ unsafe