module Test.Main where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Data.String.NonEmpty (nes)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import FusionAuth (ApiUrl(..), FusionClientError, RegistrationResponse, defaultRegistration, defaultRequest, defaultUser, mkApplicationId, registerUser, unsafeEmail)


main :: Effect Unit
main = launchAff_ do
  applicationId <- maybe (throwError $ error "ApplicationId") pure 
    $ mkApplicationId 
    $ nes (SProxy :: SProxy "63ba6d7e-af6c-44bf-9516-41c1152ab784")
  let
    registration = defaultRegistration applicationId
    email = unsafeEmail $ nes (SProxy :: SProxy "unisay@noreply.github.com")
    user = defaultUser { email = Just email } 
    request = defaultRequest user registration
    apiUrl = ApiUrl $ nes (SProxy :: SProxy "http://localhost:9011/oauth2/")
    res :: ReaderT ApiUrl (ExceptT FusionClientError Aff) RegistrationResponse
    res = registerUser request 

  response <- either (throwError <<< error <<< show) pure 
    =<< runExceptT (runReaderT res apiUrl)
  pure unit