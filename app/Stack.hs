{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, RankNTypes #-}

module Stack
( App
, runStack
, module Stack.Logger
, module Stack.Database
, module Stack.Password
, module Stack.Email
) where

import Control.Monad.Freer
import Control.Monad.IO.Class

import Stack.Logger
import Stack.Database
import Stack.Password
import Stack.Email

type App = Eff '[Email, Database, Logger, Password, IO]

runStack :: (MonadIO m) => App a -> m a
runStack = liftIO . runM
  . runPasswordBcrypt
  . runStdLogger
  . runSQLite "db.sqlite"
  . runLogEmail

