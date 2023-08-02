{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleInstances, GADTs, TypeOperators, FlexibleContexts, RecordWildCards, LambdaCase #-}

module Stack.Email
( Address(..)
, Mail(..)
, Email
, createMail
, sendMail
, runEmail
, runEmailSSL
, SMTPSettings(..)
, runLogEmail
) where

import Control.Monad.Freer
import Network.Mail.Mime
import qualified Network.HaskellNet.SMTP as E
import qualified Data.Text.Lazy as T
import Data.Text (Text)

import Stack.Logger

data Email s where
  CreateMail :: Address -> Address -> Text -> Text -> Email Mail
  SendMail   :: Mail -> Email ()

createMail :: (Member Email r) => Address -> Address -> Text -> Text -> Eff r Mail
createMail to from sub bod = send $ CreateMail to from sub bod

sendMail :: (Member Email r) => Mail -> Eff r ()
sendMail = send . SendMail


runEmail :: (Member IO r) => SMTPSettings -> Eff (Email ': r) a -> Eff r a
runEmail SMTPSettings{..} = runNat go
  where go :: Email s -> IO s
        go (CreateMail to from sub bod) = return $ simpleMail' to from sub (T.fromStrict bod)
        go (SendMail m) = do
          E.doSMTPPort smtpHost (fromIntegral smtpPort) $ \conn -> E.sendMail m conn

runEmailSSL :: (Member IO r) => SMTPSettings -> Eff (Email ': r) a -> Eff r a
runEmailSSL = undefined

runLogEmail :: (Member Logger r) => Eff (Email ': r) a -> Eff r a
runLogEmail = handleRelay pure $ \e -> (go e >>=)
  where go :: (Member Logger r) => Email s -> Eff r s
        go (CreateMail to from sub bod) = return $ simpleMail' to from sub (T.fromStrict bod)
        go (SendMail m) = logMessage $ show m

data SMTPSettings = SMTPSettings
  { smtpHost :: !String
  , smtpPort :: !Int
  }
