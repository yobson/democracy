{-# LANGUAGE TypeOperators, DataKinds #-}

module Stack.Env
( module Control.Monad.Freer.Reader
, Env
, Globals(..)
, runEnv
) where

import Control.Monad.Freer
import Control.Monad.Freer.Reader

import Servant.Auth.Server

type Env = Reader Globals

data Globals = Globals
  { glCookieCfg :: CookieSettings
  , glJwtCfg    :: JWTSettings
  }

runEnv :: Globals -> Eff (Env ': r) a -> Eff r a
runEnv g e = runReader e g
