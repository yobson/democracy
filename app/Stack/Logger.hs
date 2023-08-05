{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds, RankNTypes #-}

module Stack.Logger where

import Control.Monad.Freer

data Logger s where
  Log :: String -> Logger ()


logMessage :: Member Logger r => String -> Eff r ()
logMessage = send . Log

runStdLogger :: (Member IO r) => Eff (Logger : r) a -> Eff r a
runStdLogger = runNat go
  where go :: Logger s -> IO s
        go (Log s) = putStrLn s


runSilentLogger :: (Member IO r) => Eff (Logger : r) a -> Eff r a
runSilentLogger = runNat go
  where go :: Logger s -> IO s
        go (Log _) = return ()
