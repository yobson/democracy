{-# LANGUAGE TypeOperators, DataKinds, FlexibleContexts, GADTs #-}

module Stack.Server
( Server
, runServer
, genHeaders
) where

import Control.Monad.Freer
import Servant hiding (Server)
import Servant.Auth.Server

data Server s where
  GenHeaders :: ( AddHeader "Set-Cookie" SetCookie request withOneCookie
                  , AddHeader "Set-Cookie" SetCookie withOneCookie withTwoCookies
                  , ToJWT a)
               => CookieSettings -> JWTSettings -> a -> Server (Maybe (request -> withTwoCookies))


genHeaders :: ( AddHeader "Set-Cookie" SetCookie request withOneCookie
              , AddHeader "Set-Cookie" SetCookie withOneCookie withTwoCookies
              , ToJWT a, Member Server r)
           => CookieSettings -> JWTSettings -> a
           -> Eff r (Maybe (request -> withTwoCookies))
genHeaders cs jw token = send $ GenHeaders cs jw token


runServer :: (Member IO r) => Eff (Server ': r) a -> Eff r a
runServer = runNat go
  where go :: Server s -> IO s
        go (GenHeaders cookie jwt user) = acceptLogin cookie jwt user
