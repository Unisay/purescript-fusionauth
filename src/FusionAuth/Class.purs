module FusionAuth.Class 
  ( class FusionAuthM
  , FusionAuthError (..)
  , registerUser
  , loginUser
  , findUserByEmail
  , ServerError
  , ServerErrors
  , ErrorContext
  , ErrorContextRep
  , ResponseErrorContext
  , ErrorMessage
  ) where

import Prelude

import Affjax as AX
import Affjax.RequestBody (json)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Argonaut (class DecodeJson, Json, decodeJson, stringify, (.:), (.:?))
import Data.Array (concat, elem, fromFoldable, intercalate, mapMaybe)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either)
import Data.FormURLEncoded (fromArray, encode)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign.Object as StrMap
import FusionAuth.Data.ApiKey (ApiKey, printApiKey)
import FusionAuth.Data.ApiUrl (ApiUrl, printApiUrl)
import FusionAuth.Data.Email (Email, printEmail)
import FusionAuth.Data.User (User, decodeUser)
import FusionAuth.Data.UserId (printUserId)
import FusionAuth.Login (LoginRequest, LoginResponseF(..), LoginResponse, decodeLoginResponse, encodeLoginRequest)
import FusionAuth.Register (DuplicateField(..), RegisterRequest, RegisterResponse(..), encodeRegisterRequest)
import Record (merge)


class FusionAuthM m where
  registerUser :: RegisterRequest -> m RegisterResponse
  loginUser :: LoginRequest -> m LoginResponse
  findUserByEmail :: Email -> m (Maybe User)

type ErrorContextRep r = (status :: StatusCode, requestBody :: Maybe String | r)
type ErrorContext = {| ErrorContextRep ()}
type ResponseErrorContext = {| ErrorContextRep (responseBody :: String)}
type ErrorMessage = String

type ServerError = 
  { code :: String
  , message :: ErrorMessage
  }

newtype ServerErrors = ServerErrors
  { general :: Array ServerError 
  , fields :: StrMap.Object (Array ServerError)
  }

derive newtype instance eqServerErrors :: Eq ServerErrors
derive newtype instance showServerErrors :: Show ServerErrors
instance decodeJsonServerErrors :: DecodeJson ServerErrors where
  decodeJson json = do
    x <- decodeJson json
    fields <- x .: "fieldErrors"
    general <- fromFoldable <$> x .:? "generalErrors"
    pure $ ServerErrors { fields, general }

data FusionAuthError
  = JsonDecodingError ErrorContext ErrorMessage 
  | UnmarshallingError ResponseErrorContext ErrorMessage
  | MalformedRequestError ResponseErrorContext ServerErrors
  | AuthorizationError ResponseErrorContext
  | ActionPreventedLogin ResponseErrorContext
  | UserExpired ResponseErrorContext
  | ServerError ResponseErrorContext
  | ResponseStatusError ResponseErrorContext ErrorMessage

instance showFusionAuthError :: Show FusionAuthError where 
  show = case _ of
    JsonDecodingError context msg -> 
      "Failed to decode JSON response: " <> msg <> showContext context
    UnmarshallingError context msg -> 
      "Failed to unmarshal JSON response: " <> msg <> showContext context
    MalformedRequestError context errors -> 
      "The request was invalid and/or malformed: " 
        <> show errors <> showContext context
    AuthorizationError _ ->
      "You did not supply a valid Authorization header. \
      \The header was omitted or your API key was not valid. "
    ActionPreventedLogin _ ->
      "The user is currently in an action that has prevented login."
    UserExpired _ ->
      "The user has expired."
    ServerError _ ->
      "There was a FusionAuth server error." 
    ResponseStatusError _ msg -> 
      "Unexpected response status: " <> msg

    where showContext = show

type ApiConfigRep r = 
  ( fusionAuthApiUrl :: ApiUrl
  , fusionAuthApiKey :: ApiKey 
  | r)

instance fusionAuthMonadAff :: 
  ( MonadAff m 
  , MonadThrow FusionAuthError m
  , MonadAsk {| ApiConfigRep r} m
  ) => FusionAuthM m where

  registerUser req =
    postJson path (encodeRegisterRequest req) handleResponse
    where 
    path = "/user/registration" <> maybe "" (printUserId >>> append "/") req.user.id

    handleResponse (context@{ status: StatusCode code }) responseBody
      | code == 200 =
          decodeJson responseBody 
            # either (throwError <<< UnmarshallingError context) pure
      | code == 400 = 
          let errors@(ServerErrors { fields }) = unmarshalErrors responseBody
              fieldErrors = concat $ StrMap.values fields
              asDupField err 
                | err.code == "[duplicate]user.username" = Just UserName
                | err.code == "[duplicate]user.email" = Just UserEmail
                | otherwise = Nothing 
          in case NEA.fromArray (mapMaybe asDupField fieldErrors) of
              Just dupFields -> 
                pure $ NonUniqueUser dupFields
              Nothing -> 
                throwError $ MalformedRequestError context errors
      | code == 401 = throwError $ AuthorizationError context
      | code == 500 = throwError $ ServerError context 
      | code == 503 = throwError $ ServerError context 
      | otherwise   = throwError $ ResponseStatusError context $ 
          "FusionAuth API responded with status code " <> show code

  loginUser req = 
    postJson "/login" (encodeLoginRequest req) handleResponse

    where 
    handleResponse (context@{ status: StatusCode code }) responseBody 
      | code `elem` [200, 202, 203, 212, 242] = 
          decodeLoginResponse responseBody 
            # either (throwError <<< UnmarshallingError context) pure
      | code == 400 = throwError $ MalformedRequestError context 
        $ unmarshalErrors responseBody
      | code == 401 = throwError $ AuthorizationError context
      | code == 404 = pure LoginUnsuccessful
      | code == 409 = throwError $ ActionPreventedLogin context
      | code == 410 = throwError $ UserExpired context
      | otherwise   = throwError $ ResponseStatusError context 
        $ "FusionAuth API responded with status code " <> show code


  findUserByEmail email =
    getJson uri handleResponse

    where 
    uri = intercalate "?" ["/user", params]
    params = urlEncodeKV "email" (Just $ printEmail email)
    handleResponse (context@{ status: StatusCode code}) responseBody
      | code == 200 = 
        either (throwError <<< UnmarshallingError context) (Just >>> pure) do
          json <- decodeJson responseBody
          json .: "user" >>= decodeUser  
      | code == 400 = throwError $ MalformedRequestError context 
          $ unmarshalErrors responseBody
      | code == 401 = throwError $ AuthorizationError context
      | code == 404 = pure Nothing
      | code == 500 = throwError $ ServerError context 
      | code == 503 = throwError $ ServerError context 
      | otherwise   = throwError $ ResponseStatusError context $ 
          "FusionAuth API responded with status code " <> show code

urlEncodeKV :: String -> Maybe String -> String
urlEncodeKV k mv = encode $ fromArray [Tuple k mv]

unmarshalErrors :: Json -> ServerErrors
unmarshalErrors = either metaError identity <<< decodeJson

metaError :: String -> ServerErrors
metaError message = ServerErrors
  { general: [{ code: "unmarshalling" , message }]
  , fields: mempty 
  }

postJson :: forall a m r
   . MonadAff m
  => MonadThrow FusionAuthError m
  => MonadAsk {| ApiConfigRep r } m
  => String 
  -> Json
  -> (ResponseErrorContext -> Json -> m a)
  -> m a
postJson path requestJson responseHandler = do
  { fusionAuthApiUrl, fusionAuthApiKey } <- ask
  { status, body } <- liftAff $ AX.request $ AX.defaultRequest
    { url = printApiUrl fusionAuthApiUrl <> path
    , method = Left POST
    , responseFormat = ResponseFormat.json
    , content = Just $ json requestJson
    , headers = [RequestHeader "Authorization" $ printApiKey fusionAuthApiKey]
    }
  let 
    errorContext = { requestBody: Just (stringify requestJson), status }
  case body of
    Left err -> throwError 
      $ JsonDecodingError errorContext $ AX.printResponseFormatError err
    Right responseJson -> 
      let errorContext' = errorContext `merge` { responseBody: stringify responseJson }
      in responseHandler errorContext' responseJson

getJson :: forall a m r
   . MonadAff m
  => MonadThrow FusionAuthError m
  => MonadAsk {| ApiConfigRep r } m
  => String 
  -> (ResponseErrorContext -> Json -> m a)
  -> m a
getJson path responseHandler = do
  { fusionAuthApiUrl, fusionAuthApiKey } <- ask
  { status, body } <- liftAff $ AX.request $ AX.defaultRequest
    { url = printApiUrl fusionAuthApiUrl <> path
    , method = Left GET
    , responseFormat = ResponseFormat.json
    , headers = [RequestHeader "Authorization" $ printApiKey fusionAuthApiKey]
    }
  let 
    errorContext = { requestBody: Nothing, status }
  case body of
    Left err -> throwError 
      $ JsonDecodingError errorContext $ AX.printResponseFormatError err
    Right responseJson -> 
      let errorContext' = errorContext `merge` { responseBody: stringify responseJson }
      in responseHandler errorContext' responseJson
