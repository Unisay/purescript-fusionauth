module FusionAuth.EncryptionScheme 
  ( EncryptionScheme (..)
  ) where

import Prelude

import Data.Argonaut (class EncodeJson)
import Data.Argonaut.Core as Json
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)


data EncryptionScheme
  = SaltedMd5
  | SaltedSha256
  | SaltedHmacSha256
  | SaltedPbkdf2HmacSha256
  | Bcrypt
  | CustomEncryption String

derive instance genericEncryptionScheme :: Generic EncryptionScheme _
derive instance eqEncryptionScheme :: Eq EncryptionScheme
instance showEncryptionScheme :: Show EncryptionScheme where show = genericShow
instance encodeJsonEncryptionScheme :: EncodeJson EncryptionScheme where
  encodeJson = case _ of
    SaltedMd5 -> as "salted-md5"
    SaltedSha256 -> as "salted-sha256"
    SaltedHmacSha256 -> as "salted-hmac-sha256"
    SaltedPbkdf2HmacSha256 -> as "salted-pbkdf2-hmac-sha256"
    Bcrypt -> as "bcrypt"
    CustomEncryption ce -> as ce
    where as = Json.fromString