{-# LANGUAGE NoImplicitPrelude #-}

module Course.Id where

import Course.Core
import qualified Prelude as P

data Id a = Id a deriving (Eq, Show)

runId :: Id a -> a
runId (Id a) = a

-- e.g. 
--		runId $ mapId succ $ Id 'a' produces 'b'
--		runId $ mapId succ (Id 'a') produces 'b'
mapId :: (a -> b) -> Id a -> Id b
mapId f (Id a)    = Id (f a)

bindId :: (a -> Id b) -> Id a -> Id b
bindId f (Id a) = f a

instance P.Monad Id where
  (>>=) =
    flip bindId
  return =
    Id
