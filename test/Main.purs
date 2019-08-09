module Test.Main where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Either (either)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.String.NonEmpty (nes)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import FusionAuth as FA
import FusionAuth.Register (RegisterResponse(..))


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
      { email = Just email
      , password = Just password
      , username = FA.mkUsername (nes (SProxy :: SProxy "unisay"))
      }

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
      registration = reg { timezone = europeBerlin }

    registrationRes <- FA.registerUser $
      (FA.defaultRegisterRequest user registration) 
        { generateAuthenticationToken = Just false }

    case registrationRes of
      NonUniqueUser duplicateFields ->
        log $ "User data is not unique: " <> show duplicateFields
      UserRegistered _ -> do
        log "User registered succesfully!"

        let wrongEmail = FA.unsafeEmail "nouser@noreply.github.com"
          
        whenM (FA.findUserByEmail wrongEmail <#> isJust)
          (liftEffect $ throw "User was found by wrong email")

        whenM (FA.findUserByEmail email <#> isNothing)
          (liftEffect $ throw "User not found by email")

        void $ FA.loginUser (FA.defaultLoginRequest email password)
          { applicationId = Just applicationId }