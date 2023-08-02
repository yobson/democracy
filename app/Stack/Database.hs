{-# LANGUAGE GADTs, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE TypeOperators, OverloadedLabels #-}
{-# LANGUAGE DataKinds, RankNTypes #-}

module Stack.Database 
( getEmails
, emailExists
, newUser
, unlockUser
, lockUser
, activateUser
, getLoginData
, runSQLite
, Database
, AccountState(..)
) where

import Control.Monad.Freer
import Database.Selda
import Database.Selda.SQLite
import Data.Functor
import Data.ByteString.Base64
import System.Random

data Database s where
  GetEmails    :: Database [Text]
  EmailExists  :: Text -> Database Bool
  NewUser      :: Text -> Text -> AccountState -> Database (Maybe Text)
  UnlockUser   :: Text -> Database Bool
  LockUser     :: Text -> Database Bool
  ActivateUser :: Text -> Text -> Database Bool
  GetLoginData :: Text -> Database (Maybe (Text,AccountState))


getEmails :: Member Database r => Eff r [Text]
getEmails = send GetEmails

emailExists :: Member Database r => Text -> Eff r Bool
emailExists = send . EmailExists

newUser :: Member Database r => Text -> Text -> AccountState -> Eff r (Maybe Text)
newUser u p s = send $ NewUser u p s

unlockUser :: Member Database r => Text -> Eff r Bool
unlockUser = send . UnlockUser

lockUser :: Member Database r => Text -> Eff r Bool
lockUser = send . LockUser

activateUser :: Member Database r => Text -> Text -> Eff r Bool
activateUser addr token = send $ ActivateUser addr token

getLoginData :: Member Database r => Text -> Eff r (Maybe (Text,AccountState))
getLoginData = send . GetLoginData

runSQLite :: (Member IO r) => FilePath -> Eff (Database ': r) a -> Eff r a
runSQLite db = runNat (withSQLite db . go')
  where go' s = tryCreateTable users >> go s

go :: Database s -> SeldaT d IO s
go GetEmails = query $ do
  user <- select users
  return (user ! #email)

go (EmailExists e) = do
  sameEmail <- query $ do
    user <- select users
    restrict (user ! #email .== text e)
    return (user ! #email)
  return $ not $ null sameEmail

go (NewUser u p s) = do
  noInsert <- go (EmailExists u)
  if noInsert
     then return Nothing
     else do
       seed <- initStdGen
       let (token, _) = genByteString 32 seed
           b64Token   = encodeBase64 token
       insert_ users [User def u p s b64Token] $> Just b64Token

go (UnlockUser mail) = 
  (== 1) <$> update users
    (\user -> user ! #email .== text mail)
    (\user -> user `with` [#state := literal Confirmed])

go (LockUser mail) =
  (== 1) <$> update users
    (\user -> user ! #email .== text mail)
    (\user -> user `with` [#state := literal Locked])

go (ActivateUser mail token) =
  (== 1) <$> update users
    (\user -> (user ! #email .== text mail) .&& (user ! #conf .== text token))
    (\user -> user `with` [#state := literal Confirmed])

go (GetLoginData mail) = do
  user <- query $ do
    user <- select users
    restrict (user ! #email .== text mail)
    return (user ! #pass :*: user ! #state)
  case user of
    []    -> return Nothing
    ((p :*: s):_) -> return $ Just (p, s)



---------- DB Stuff

data User = User
  { pid   :: ID User
  , email :: !Text
  , pass  :: !Text
  , state :: !AccountState
  , conf  :: !Text
  } deriving Generic

data AccountState = Confirmed | Locked
  deriving (Show, Read, Bounded, Enum, Eq)

instance SqlRow User
instance SqlType AccountState

users :: Table User
users = table "users" [#pid :- autoPrimary]
