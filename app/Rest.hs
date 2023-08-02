{-# LANGUAGE DataKinds, RecordWildCards #-}
{-# LANGUAGE KindSignatures, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Rest where

import Servant hiding (serveDirectoryWebApp)
import Servant.RawM.Server


import Api
import Api.Types
import Stack
import Data.Text (Text)
import Data.Char

import qualified Data.Text      as T
import qualified Pages.Welcome  as W
import qualified Pages.Register as R
import qualified Pages.SignIn   as S

democracyServer :: ServerT API App
democracyServer =  privateApi 
              :<|> registerApi
              :<|> verifyApi
              :<|> signInApi
              :<|> static


privateApi :: ServerT PrivateAPI App
privateApi _ =  welcomePage
           :<|> createApi
           :<|> electionsApi
           :<|> signOut

registerApi :: ServerT RegAPI App
registerApi =  registerPage
          :<|> registerPost

verifyApi :: ServerT VerifyAPI App
verifyApi = verifyGet

signInApi :: ServerT SignInAPI App
signInApi =  signInPage
        :<|> signInPost

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
         Nothing -> return R.pageData {R.errorMsg = "!Error! Maybe email already registered"}

verifyGet :: Text -> Text -> App (Either W.WelcomeP S.SignInP)
verifyGet (lower -> email) token = do
  activated <- activateUser email token
  if activated
     then return $ Right $ S.pageData { S.errMsg = "Account activated! You can now log in!" }
     else return $ Left  $ W.pageData { W.errMsg = "!Failed to activate account!" }


signInPage :: App S.SignInP
signInPage = return S.pageData

signInPost :: SignInData -> App (Either S.SignInP W.WelcomeP)
signInPost SignInData{..} = do
  ps <- getLoginData (lower siEmail)
  case ps of
    Nothing -> return $ Left $ S.pageData { S.errMsg = "!Incorrect Email or Password" }
    Just (pass,state) -> do
      ck <- checkPassword siPass pass
      if not ck
         then return $ Left $ S.pageData { S.errMsg = "!Incorrect Email or Password" }
         else if state == Locked
         then return $ Left $ S.pageData { S.errMsg = "!You need to activate your account" }
         else return $ Right $ W.pageData { W.errMsg = "Signed in!" }


createApi = undefined

electionsApi = undefined

signOut = undefined

static :: ServerT StaticAPI App
static = serveDirectoryWebApp "static"

lower :: Text -> Text
lower = T.map toLower
