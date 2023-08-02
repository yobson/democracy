{-# LANGUAGE GADTs, TypeOperators, FlexibleContexts, DataKinds #-}

module Stack.Password
( Password
, ClearPass
, HashPass
, hashPassword
, checkPassword
, runPasswordBcrypt
) where

import qualified Data.Password.Bcrypt as B
import Control.Monad.Freer
import Data.Text (Text)

type ClearPass = Text
type HashPass  = Text

data Password s where
  HashPassword :: ClearPass -> Password HashPass
  CheckPassword :: ClearPass -> HashPass -> Password Bool

hashPassword :: (Member Password r) => ClearPass -> Eff r HashPass
hashPassword = send . HashPassword

checkPassword :: (Member Password r) => ClearPass -> HashPass -> Eff r Bool
checkPassword clear hash = send $ CheckPassword clear hash


runPasswordBcrypt :: (Member IO r) => Eff (Password ': r) a -> Eff r a
runPasswordBcrypt = runNat go
  where go :: Password s -> IO s
        go (HashPassword clear) =
            B.unPasswordHash <$> B.hashPassword  (B.mkPassword clear)
        go (CheckPassword clear hash) = 
          let pass = B.mkPassword clear  in
          let cryp = B.PasswordHash hash in
          case B.checkPassword pass cryp of
            B.PasswordCheckSuccess -> return True
            B.PasswordCheckFail    -> return False
