{-# LANGUAGE DataKinds, RecordWildCards, FlexibleContexts #-}
{-# LANGUAGE KindSignatures, FlexibleInstances, MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Rest where

import Servant hiding (serveDirectoryWebApp, Server, throwError)
import Servant.Auth.Server
import Servant.RawM.Server

import Control.Monad.Freer

import Api
import Api.Types
import Stack
import Data.Text (Text)
import Data.Char

import qualified Data.Text      as T
import qualified Pages.Welcome  as W
import qualified Pages.Register as R
import qualified Pages.SignIn   as S
import qualified Pages.Create   as C

democracyServer :: ServerT API App
democracyServer =  privateApi 
              :<|> registerApi
              :<|> verifyApi
              :<|> signInApi
              :<|> static

throwUnAuth :: (Member AppErr r) => Eff r a
throwUnAuth = throwPage 401 S.pageData{S.errMsg = "!You need to sign in to view this page!"}

privateApi :: ServerT PrivateAPI App
privateApi (Authenticated u) =  welcomePageAuth u
                           :<|> createApi u
                           :<|> electionsApi
                           :<|> signOut
privateApi _                 = welcomePage
                           :<|> throwUnAuth
                           :<|> throwUnAuth
                           :<|> throwUnAuth

registerApi :: ServerT RegAPI App
registerApi =  registerPage
          :<|> registerPost

verifyApi :: ServerT VerifyAPI App
verifyApi = verifyGet

signInApi :: ServerT SignInAPI App
signInApi =  signInPage
        :<|> signInPost

welcomePageAuth :: User -> App W.WelcomeP
welcomePageAuth usr = do
  logMessage "Welcome Page"
  return W.pageData{ W.user = Just usr }

welcomePage :: App W.WelcomeP
welcomePage = do
  logMessage "Welcome Page"
  return W.pageData

registerPage :: App R.RegisterP
registerPage = do
  logMessage "Register Page"
  return R.pageData

registerPost :: RegData -> App R.RegisterP
registerPost RegData{..} 
  | pass1 /= pass2     = return R.pageData { R.errorMsg = "!Passwords do not match!" }
  | T.length pass1 < 8 = return R.pageData { R.errorMsg = "!Password too short, Must be at least 8 characters long"} 
  | otherwise          = do
      let lmail = lower email
      hash <- hashPassword pass1
      link <- newUser lmail hash Locked
      case link of
         Just token -> do
           mail <- createMail (Address Nothing lmail) 
                      "no-replay@domain"
                      "Democracy: Confirm Email"
                      ("Click this link: " 
                        <> "http://localhost:8080/verify?email=" <> email
                        <> "&token=" <> token)
           sendMail mail
           return R.pageData {R.errorMsg = "An email has been sent. Click on the link to verify"}
         Nothing -> throwPage 400 R.pageData {R.errorMsg = "!Error! Maybe email already registered"}

verifyGet :: Text -> Text -> App S.SignInP
verifyGet (lower -> email) token = do
  activated <- activateUser email token
  if activated
     then return $ S.pageData { S.errMsg = "Account activated! You can now log in!" }
     else throwPage 400 W.pageData { W.errMsg = "!Failed to activate account!" }


signInPage :: App S.SignInP
signInPage = return S.pageData

signInPost :: SignInData -> App (LogInCookies W.WelcomeP)
signInPost SignInData{..} = do
  ps <- getLoginData (lower siEmail)
  case ps of
    Nothing -> throwPage 401 S.pageData{ S.errMsg = "!Incorrect Email or Password" }
    Just (pass,state) -> do
      ck <- checkPassword siPass pass
      if not ck
         then throwPage 401 S.pageData{ S.errMsg = "!Incorrect Email or Password" }
         else if state == Locked
         then throwPage 401 S.pageData { S.errMsg = "!You need to activate your account" }
         else do
           cookieCfg <- asks glCookieCfg
           jwtCfg    <- asks glJwtCfg
           let user  = User{usrEmail = lower siEmail}
           applyHeaders <- genHeaders cookieCfg jwtCfg user
           case applyHeaders of
             Nothing -> throwError err401
             Just fs -> return $ fs W.pageData{ W.errMsg = "Signed in!", W.user = Just user }

createApi :: User -> App C.CreateP
createApi u = return C.pageData{C.user=u}

electionsApi = undefined

signOut = undefined

static :: ServerT StaticAPI App
static = serveDirectoryWebApp "static"

lower :: Text -> Text
lower = T.map toLower
