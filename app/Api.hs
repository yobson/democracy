{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Api where

import Servant
import Servant.Auth
import Servant.Auth.Server (SetCookie)
import Servant.RawM
import Servant.HTML.Lucid
import Data.Text (Text)


import Api.Types


import qualified Pages.Welcome  as W
import qualified Pages.Register as R
import qualified Pages.SignIn   as S


type API =  PrivateAPI
       :<|> "register" :> RegAPI
       :<|> "verify"   :> VerifyAPI
       :<|> "signin"   :> SignInAPI
       :<|> "static"   :> StaticAPI

type RegAPI =  Get '[HTML] R.RegisterP
          :<|> ReqBody '[FormUrlEncoded] RegData :> Post '[HTML] R.RegisterP

type VerifyAPI = QueryParam' [Required, Strict] "email" Text :> QueryParam' [Required, Strict] "token" Text :> Get '[HTML] S.SignInP

type SignInAPI =  Get '[HTML] S.SignInP
             :<|> ReqBody '[FormUrlEncoded] SignInData :> Post '[HTML] (LogInCookies W.WelcomeP)
         

type PrivateAPI = Auth '[Cookie] User :> MainApi

type MainApi =  Get '[HTML] W.WelcomeP
           :<|> "create"       :> Get '[HTML] W.WelcomeP
           :<|> "my-elections" :> Get '[HTML] W.WelcomeP
           :<|> "signout"      :> Get '[HTML] W.WelcomeP
       

type StaticAPI = RawM

type LogInCookies a = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] a
