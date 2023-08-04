{-# LANGUAGE TypeApplications #-}

module Main where

import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server

import Api
import Rest
import Stack

main :: IO ()
main = do
  myKey <- generateKey
  let cookieCfg = defaultCookieSettings 
                    { cookieIsSecure    = NotSecure
                    , cookieXsrfSetting = Nothing
                    }
      jwtCfg  = defaultJWTSettings myKey
      globals = Globals
                  { glCookieCfg = cookieCfg
                  , glJwtCfg    = jwtCfg
                  }
  run 8080 $ serveWithContextT (Proxy @API)
    (cookieCfg :. jwtCfg :. EmptyContext)
    (runStack globals)
    democracyServer 
