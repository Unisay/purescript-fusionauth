module FusionAuth.Data.UserId 
  ( UserId (..)
  , mkUserId
  , printUserId
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Core as Json
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.UUID (UUID)
import Data.UUID as UUID


newtype UserId = UserId UUID

derive instance genericUserId :: Generic UserId _
derive instance eqUserId :: Eq UserId
instance showUserId :: Show UserId where show = genericShow

instance encodeJsonUserId :: EncodeJson UserId where
  encodeJson (UserId uuid) = Json.fromString (UUID.toString uuid)

instance decodeJsonUserId :: DecodeJson UserId where
  decodeJson json = 
    UserId <$> note "Failed to decode UserId" mbUuid
    where mbUuid = Json.toString json >>= UUID.parseUUID


mkUserId :: String -> Maybe UserId
mkUserId str | parsed@(Just uuid) <- UUID.parseUUID str = 
  parsed <#> UserId
mkUserId _ = Nothing

printUserId :: UserId -> String
printUserId (UserId uuid) = UUID.toString uuid

