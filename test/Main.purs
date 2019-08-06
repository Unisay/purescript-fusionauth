module Test.Main where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Either (either)
import Data.Lens (set)
import Data.Maybe (Maybe(..), maybe)
import Data.String.NonEmpty (nes)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import FusionAuth (_applicationId, _email, _generateAuthenticationToken, _password, _timezone, _username)
import FusionAuth as FA


main :: Effect Unit
main = launchAff_ do
  applicationId <- maybe (throwError $ error "ApplicationId") pure 
    $ FA.mkApplicationId 
    $ nes (SProxy :: SProxy "f4876cf7-befa-47fe-b9da-58de0179d187")
  let
    registration = FA.defaultRegistration applicationId
    email = FA.unsafeEmail "unisay@noreply.github.com"
    password = FA.unsafePassword "password"
    user = FA.defaultUserOut 
      # _email `set` Just email
      # _password `set` Just password
      # _username `set` FA.mkUsername (nes (SProxy :: SProxy "unisay"))

    config = 
      { fusionAuthApiUrl: FA.mkApiUrl 
        $ nes (SProxy :: SProxy "http://localhost:9011/api") 
      , fusionAuthApiKey: FA.mkApiKey 
        $ nes (SProxy :: SProxy "joeyL9RcHmc2ND7d52FPyLHisqNuiT882xDtx7ebZTc")
      }

  void $ registerLogin user registration email password applicationId
    # flip runReaderT config 
    # runExceptT >>= either (throwError <<< error <<< show) pure


  where

  registerLogin user reg email password applicationId = do
    let 
      europeBerlin = FA.mkTimezone $ nes (SProxy :: _ "Europe/Berlin")
      registration = set _timezone europeBerlin reg 

    FA.RegisterResponse _ <- FA.registerUser 
      $ FA.defaultRegisterRequest user registration
        # _generateAuthenticationToken `set` Just false 

    void $ FA.loginUser $ FA.defaultLoginRequest email password
      # _applicationId `set` Just applicationId