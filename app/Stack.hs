{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, RankNTypes #-}

module Stack
( App
, runStack
, runStack'
, module Stack.Logger
, module Stack.Database
, module Stack.Password
, module Stack.Email
, module Stack.Env
, module Stack.Server
, module Stack.Error
, module Control.Monad.Freer.Exception
) where

import Control.Monad.Freer
import Control.Monad.Freer.Exception
import Control.Monad.IO.Class
import qualified Control.Monad.Except as E

import Servant (ServerError)

import Stack.Logger
import Stack.Database
import Stack.Password
import Stack.Email
import Stack.Env
import Stack.Server
import Stack.Error

type App = Eff '[Env, Email, Database, Logger, Password, Server, AppErr, IO]

runStack :: (MonadIO m, E.MonadError ServerError m) => Globals -> App a -> m a
runStack g xm = runStack' g xm >>= either E.throwError return

runStack' :: (MonadIO m, E.MonadError ServerError m) => Globals -> App a -> m (Either ServerError a)
runStack' g = liftIO . runM
  . runError
  . runServer
  . runPasswordBcrypt
  . runStdLogger
  . runSQLite "db.sqlite"
  . runLogEmail
  . runEnv g

