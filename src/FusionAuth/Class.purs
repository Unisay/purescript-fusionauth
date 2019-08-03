module FusionAuth.Class 
  ( class FusionAuthM
  , FusionClientError (..)
  , registerUser
  ) where

import Prelude

import Affjax as AX
import Affjax.RequestBody (json)
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Aff.Class (class MonadAff, liftAff)
import FusionAuth.ApiUrl (ApiUrl, unApiUrl)
import FusionAuth.Registration (RegistrationRequest, RegistrationResponse)

class FusionAuthM m where
  registerUser :: RegistrationRequest -> m RegistrationResponse

data FusionClientError
  = JsonDecodingError String
  | UnmarshallingError String
derive instance genericFusionClientError :: Generic FusionClientError _
instance showFusionClientError :: Show FusionClientError where show = genericShow

instance fusionAuthMonadAff :: 
  ( MonadAff m
  , MonadAsk ApiUrl m
  , MonadThrow FusionClientError m
  ) => FusionAuthM m where
  registerUser req = do
    url <- asks unApiUrl
    let body = json (encodeJson req)
    res <- liftAff $ AX.post ResponseFormat.json url body
    case res.body of
      Left err ->
        throwError $ JsonDecodingError $ AX.printResponseFormatError err
      Right json ->
        either (throwError <<< UnmarshallingError) pure (decodeJson json)
