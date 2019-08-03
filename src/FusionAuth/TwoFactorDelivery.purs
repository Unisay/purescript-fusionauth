module FusionAuth.TwoFactorDelivery
  ( TwoFactorDelivery (..)
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Core as Json
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String (toLower)


data TwoFactorDelivery
  = None
  | TextMessage

derive instance eqTwoFactorDelivery :: Eq TwoFactorDelivery
derive instance genericTwoFactorDelivery :: Generic TwoFactorDelivery _
instance showTwoFactorDelivery :: Show TwoFactorDelivery where 
  show = genericShow
instance encodeJsonTwoFactorDelivery :: EncodeJson TwoFactorDelivery where
  encodeJson = case _ of
    None -> Json.fromString "None"
    TextMessage -> Json.fromString "TextMessage"
instance decodeJsonTwoFactorDelivery :: DecodeJson TwoFactorDelivery where
  decodeJson json = note "Invalid TwoFactorDelivery value" $ 
    Json.toString json >>= \tfd -> case toLower tfd of
      "none" -> pure None
      "textmessage" -> pure TextMessage
      _ -> Nothing

