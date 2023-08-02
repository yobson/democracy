{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Api where

import Servant
import Servant.RawM
import Servant.HTML.Lucid
import Lucid
import Data.Text (Text)


import Api.Types


import qualified Pages.Welcome  as W
import qualified Pages.Register as R
import qualified Pages.SignIn   as S


type API =  PublicAPI :<|> StaticAPI

instance (ToHtml a, ToHtml b) => ToHtml (Either a b) where
  toHtmlRaw (Left x) = toHtmlRaw x
  toHtmlRaw (Right x) = toHtmlRaw x
  toHtml (Left x) = toHtml x
  toHtml (Right x) = toHtml x


type PublicAPI =  Get '[HTML] W.WelcomeP
             :<|> "register" :> Get '[HTML] R.RegisterP
             :<|> "register" :> ReqBody '[FormUrlEncoded] RegData :> Post '[HTML] R.RegisterP
             :<|> "verify"   :> QueryParam' [Required, Strict] "email" Text :> QueryParam' [Required, Strict] "token" Text :> Get '[HTML] (Either W.WelcomeP S.SignInP)
             :<|> "signin"   :> Get '[HTML] S.SignInP
             :<|> "signin"   :> ReqBody '[FormUrlEncoded] SignInData :> Post '[HTML] (Either S.SignInP W.WelcomeP)
       

type StaticAPI = "static" :> RawM

