{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Api where

import Servant
import Servant.Auth
import Servant.RawM
import Servant.HTML.Lucid
import Lucid
import Data.Text (Text)


import Api.Types


import qualified Pages.Welcome  as W
import qualified Pages.Register as R
import qualified Pages.SignIn   as S


instance (ToHtml a, ToHtml b) => ToHtml (Either a b) where
  toHtmlRaw (Left x) = toHtmlRaw x
  toHtmlRaw (Right x) = toHtmlRaw x
  toHtml (Left x) = toHtml x
  toHtml (Right x) = toHtml x


type API =  PrivateAPI
       :<|> "register" :> RegAPI
       :<|> "verify"   :> VerifyAPI
       :<|> "signin"   :> SignInAPI
       :<|> "static"   :> StaticAPI

type RegAPI =  Get '[HTML] R.RegisterP
          :<|> ReqBody '[FormUrlEncoded] RegData :> Post '[HTML] R.RegisterP

type VerifyAPI = QueryParam' [Required, Strict] "email" Text :> QueryParam' [Required, Strict] "token" Text :> Get '[HTML] (Either W.WelcomeP S.SignInP)

type SignInAPI =  Get '[HTML] S.SignInP
             :<|> ReqBody '[FormUrlEncoded] SignInData :> Post '[HTML] (Either S.SignInP W.WelcomeP)
         

type PrivateAPI = Auth '[Cookie] User :> MainApi

type MainApi =  Get '[HTML] W.WelcomeP
           :<|> "create"       :> Get '[HTML] W.WelcomeP
           :<|> "my-elections" :> Get '[HTML] W.WelcomeP
           :<|> "signout"      :> Get '[HTML] W.WelcomeP
       

type StaticAPI = RawM

