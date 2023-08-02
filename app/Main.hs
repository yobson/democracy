{-# LANGUAGE TypeApplications #-}

module Main where

import Network.Wai.Handler.Warp
import Servant

import Api
import Rest
import Stack

main :: IO ()
main = run 8080 $ serveWithContextT (Proxy @API)
  EmptyContext 
  runStack
  democracyServer 
