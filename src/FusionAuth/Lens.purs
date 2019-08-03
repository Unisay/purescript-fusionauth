module FusionAuth.Lens 
  ( _birthDate
  , _data
  , _email
  , _encryptionScheme
  , _expiry
  , _factor
  , _firstName
  , _fullName
  , _id
  , _imageUrl
  , _lastName
  , _middleName
  , _mobilePhone
  , _password
  , _passwordChangeRequired
  , _preferredLanguages
  , _timezone
  , _twoFactorEnabled
  , _twoFactorSecret
  , _user
  , _username
  , _generateAuthenticationToken
  , _loginId
  , _applicationId
  , _noJWT
  ) where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))

_birthDate :: forall s a r. Newtype s { birthDate :: a | r } => Lens' s a
_birthDate = _Newtype <<< prop (SProxy :: _ "birthDate")

_data :: forall s a r. Newtype s  { "data" :: a | r } => Lens' s a
_data= _Newtype <<< prop (SProxy :: _ "data")

_email :: forall s a r. Newtype s { email :: a | r } => Lens' s a
_email = _Newtype <<< prop (SProxy :: _ "email")

_encryptionScheme :: forall s a r. Newtype s { encryptionScheme :: a | r } => Lens' s a
_encryptionScheme = _Newtype <<< prop (SProxy :: _ "encryptionScheme")

_expiry :: forall s a r. Newtype s { expiry :: a | r } => Lens' s a
_expiry = _Newtype <<< prop (SProxy :: _ "expiry")

_factor :: forall s a r. Newtype s { factor :: a | r } => Lens' s a
_factor = _Newtype <<< prop (SProxy :: _ "factor")

_firstName :: forall s a r. Newtype s { firstName :: a | r } => Lens' s a
_firstName = _Newtype <<< prop (SProxy :: _ "firstName")

_fullName :: forall s a r. Newtype s { fullName :: a | r } => Lens' s a
_fullName = _Newtype <<< prop (SProxy :: _ "fullName")

_id :: forall s a r. Newtype s { id :: a | r } => Lens' s a
_id = _Newtype <<< prop (SProxy :: _ "id")

_imageUrl :: forall s a r. Newtype s { imageUrl :: a | r } => Lens' s a
_imageUrl = _Newtype <<< prop (SProxy :: _ "imageUrl")

_lastName :: forall s a r. Newtype s { lastName :: a | r } => Lens' s a
_lastName = _Newtype <<< prop (SProxy :: _ "lastName")

_middleName :: forall s a r. Newtype s { middleName :: a | r } => Lens' s a
_middleName = _Newtype <<< prop (SProxy :: _ "middleName")

_mobilePhone :: forall s a r. Newtype s { mobilePhone :: a | r } => Lens' s a
_mobilePhone = _Newtype <<< prop (SProxy :: _ "mobilePhone")

_password :: forall s a r. Newtype s { password :: a | r } => Lens' s a
_password = _Newtype <<< prop (SProxy :: _ "password")

_passwordChangeRequired :: forall s a r. Newtype s { passwordChangeRequired :: a | r } => Lens' s a
_passwordChangeRequired = _Newtype <<< prop (SProxy :: _ "passwordChangeRequired")

_preferredLanguages :: forall s a r. Newtype s { preferredLanguages :: a | r } => Lens' s a
_preferredLanguages = _Newtype <<< prop (SProxy :: _ "preferredLanguages")

_timezone :: forall s a r. Newtype s { timezone :: a | r } => Lens' s a
_timezone = _Newtype <<< prop (SProxy :: _ "timezone")

_twoFactorEnabled :: forall s a r. Newtype s { twoFactorEnabled :: a | r } => Lens' s a
_twoFactorEnabled = _Newtype <<< prop (SProxy :: _ "twoFactorEnabled")

_twoFactorSecret :: forall s a r. Newtype s { twoFactorSecret :: a | r } => Lens' s a
_twoFactorSecret = _Newtype <<< prop (SProxy :: _ "twoFactorSecret")

_user :: forall s a r. Newtype s { user :: a | r } => Lens' s a
_user = _Newtype <<< prop (SProxy :: _ "user")

_username :: forall s a r. Newtype s { username :: a | r } => Lens' s a
_username = _Newtype <<< prop (SProxy :: _ "username")

_generateAuthenticationToken :: 
  forall s a r. Newtype s { generateAuthenticationToken :: a | r } => Lens' s a
_generateAuthenticationToken = 
  _Newtype <<< prop (SProxy :: _ "generateAuthenticationToken")

_loginId :: forall s a r. Newtype s { loginId :: a | r } => Lens' s a
_loginId = _Newtype <<< prop (SProxy :: _ "loginId")

_applicationId :: forall s a r. Newtype s { applicationId :: a | r } => Lens' s a
_applicationId = _Newtype <<< prop (SProxy :: _ "applicationId")

_noJWT :: forall s a r. Newtype s { noJWT :: a | r } => Lens' s a
_noJWT = _Newtype <<< prop (SProxy :: _ "noJWT")
