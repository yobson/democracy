{-# LANGUAGE FlexibleContexts, OverloadedStrings, MonoLocalBinds #-}

module Stack.Error where

import Lucid

import Control.Monad.Freer
import Control.Monad.Freer.Exception

import Servant (ServerError(..))

type AppErr = Exc ServerError

throwPage :: (Member AppErr r, ToHtml p) => Int -> p -> Eff r a
throwPage code body = throwError $
  ServerError 
    { errHTTPCode = code
    , errReasonPhrase = ""
    , errHeaders = [ ("Content-Type", "text/html;charset=utf-8") ]
    , errBody = renderBS $ toHtml body
    }
