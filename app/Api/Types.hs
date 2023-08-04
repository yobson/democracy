{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Api.Types where

import Data.Text
import GHC.Generics
import Web.FormUrlEncoded
import Servant.Auth.JWT
import Data.Aeson

data RegData = RegData
  { email :: !Text
  , pass1 :: !Text
  , pass2 :: !Text
  }

instance FromForm RegData where
  fromForm f = RegData
    <$> parseUnique "email" f
    <*> parseUnique "password1" f
    <*> parseUnique "password2" f


data SignInData = SignInData
  { siEmail :: !Text
  , siPass  :: !Text
  }

instance FromForm SignInData where
  fromForm f = SignInData
    <$> parseUnique "email" f
    <*> parseUnique "password" f


newtype User = User { usrEmail :: Text }
  deriving Generic

instance ToJSON User
instance FromJSON User
instance ToJWT User
instance FromJWT User
