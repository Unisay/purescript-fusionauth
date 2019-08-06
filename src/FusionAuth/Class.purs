module FusionAuth.Class 
  ( class FusionAuthM
  , FusionAuthError (..)
  , registerUser
  , loginUser
  , ServerError
  , ServerErrors
  , ErrorContext
  , ErrorContextRep
  , ResponseErrorContext
  , ErrorMessage
  , DuplicateField
  ) where

import Prelude

import Affjax as AX
import Affjax.RequestBody (json)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonParser, stringify, (.:?), (.:))
import Data.Array (concat, fromFoldable, mapMaybe)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), maybe)
import Data.Semigroup.Foldable (intercalate)
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign.Object as StrMap
import FusionAuth.Data.ApiKey (ApiKey, printApiKey)
import FusionAuth.Data.ApiUrl (ApiUrl, printApiUrl)
import FusionAuth.Lens (_id, _user)
import FusionAuth.Login (LoginRequest, LoginResponse)
import FusionAuth.Register (RegisterRequest, RegisterResponse)
import FusionAuth.Data.UserId (printUserId)
import Record (merge)

class FusionAuthM m where
  registerUser :: RegisterRequest -> m RegisterResponse
  loginUser :: LoginRequest -> m LoginResponse

type ErrorContextRep r = (status :: StatusCode, requestBody :: String | r)
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

data DuplicateField = UserName | UserEmail

derive instance eqDuplicateField :: Eq DuplicateField
derive instance genericDuplicateField :: Generic DuplicateField _
instance showDuplicateField :: Show DuplicateField where show = genericShow

data FusionAuthError
  = JsonDecodingError ErrorContext ErrorMessage 
  | UnmarshallingError ResponseErrorContext ErrorMessage
  | MalformedRequestError ResponseErrorContext ServerErrors
  | AuthorizationError ResponseErrorContext
  | UserNotFoundByCredentials ResponseErrorContext
  | ActionPreventedLogin ResponseErrorContext
  | DuplicateFields (NonEmptyArray DuplicateField) 
  | UserExpired ResponseErrorContext
  | ServerError ResponseErrorContext
  | ResponseStatusError ResponseErrorContext ErrorMessage

instance showFusionAuthError :: Show FusionAuthError where 
  show = case _ of
    JsonDecodingError ctx msg -> 
      "Failed to decode JSON response: " <> msg <> showContext ctx
    UnmarshallingError ctx msg -> 
      "Failed to unmarshal JSON response: " <> msg <> showContext ctx
    MalformedRequestError ctx errors -> 
      "The request was invalid and/or malformed: " 
        <> show errors <> showContext ctx
    AuthorizationError _ ->
      "You did not supply a valid Authorization header. \
      \The header was omitted or your API key was not valid. "
    UserNotFoundByCredentials _ ->
      "The user was not found or the password was incorrect."
    ActionPreventedLogin _ ->
      "The user is currently in an action that has prevented login."
    UserExpired _ ->
      "The user has expired."
    DuplicateFields dups -> intercalate "; " $ dups <#> case _ of
      UserName -> "User with such username already registered"
      UserEmail -> "User with such e-mail already registered"
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

  registerUser req = do
    let 
      path = "/user/registration" 
        <> maybe "" (printUserId >>> append "/") (req ^. _user <<< _id)
    postJson path req \ctx@{ status: StatusCode code, responseBody } -> 
      case code of
        200 -> Nothing
        400 -> Just $ 
          let errors@(ServerErrors { fields }) = unmarshalErrors responseBody
              fieldErrors = concat $ StrMap.values fields
              asDupField err 
                | err.code == "[duplicate]user.username" = Just UserName
                | err.code == "[duplicate]user.email" = Just UserEmail
                | otherwise = Nothing 
          in NEA.fromArray (mapMaybe asDupField fieldErrors) 
               # maybe (MalformedRequestError ctx errors) DuplicateFields
        401 -> Just $ AuthorizationError ctx
        500 -> Just $ ServerError ctx 
        503 -> Just $ ServerError ctx 
        cod -> Just $ ResponseStatusError ctx $ 
          "FusionAuth API responded with status code " <> show cod

  loginUser req = postJson "/login" req 
    \ctx@{ status: StatusCode code, responseBody } -> case code of
      200 -> Nothing
      202 -> Nothing
      203 -> Nothing
      212 -> Nothing
      242 -> Nothing
      400 -> Just $ MalformedRequestError ctx $ unmarshalErrors responseBody
      401 -> Just $ AuthorizationError ctx
      404 -> Just $ UserNotFoundByCredentials ctx
      409 -> Just $ ActionPreventedLogin ctx
      410 -> Just $ UserExpired ctx
      cod -> Just <<< ResponseStatusError ctx $ 
        "FusionAuth API responded with status code " <> show cod

unmarshalErrors :: String -> ServerErrors
unmarshalErrors = either metaError identity <<< (decodeJson <=< jsonParser)

metaError :: String -> ServerErrors
metaError message = ServerErrors
  { general: [{ code: "unmarshalling" , message }]
  , fields: mempty 
  }

postJson :: forall a m e r
   . EncodeJson e 
  => DecodeJson a
  => MonadAff m
  => MonadThrow FusionAuthError m
  => MonadAsk {| ApiConfigRep r } m
  => String 
  -> e 
  -> (ResponseErrorContext -> Maybe FusionAuthError) 
  -> m a
postJson path requestPayload errorHandler = do
  { fusionAuthApiUrl, fusionAuthApiKey } <- ask
  let requestJson = encodeJson requestPayload
  { status, body } <- liftAff $ AX.request $ AX.defaultRequest
    { url = printApiUrl fusionAuthApiUrl <> path
    , method = Left POST
    , responseFormat = ResponseFormat.json
    , content = Just $ json requestJson
    , headers = [RequestHeader "Authorization" $ printApiKey fusionAuthApiKey]
    }
  let errorContext = { requestBody: stringify requestJson, status }
  case body of
    Left err -> throwError 
      $ JsonDecodingError errorContext
      $ AX.printResponseFormatError err
    Right responseJson -> do
      let ctx = errorContext `merge` { responseBody: stringify responseJson }
      for_ (errorHandler ctx) throwError
      either (throwError <<< UnmarshallingError ctx) 
        pure (decodeJson responseJson)

