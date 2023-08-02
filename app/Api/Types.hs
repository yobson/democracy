{-# LANGUAGE OverloadedStrings #-}

module Api.Types where

import Data.Text
import Web.FormUrlEncoded

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
