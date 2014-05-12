{-# LANGUAGE GADTs,RankNTypes,DeriveFunctor #-}

module Faceted.FIORef (
  FIORef,
  newFIORef,
  readFIORef,
  writeFIORef,
  ) where

import Faceted.Internal

import Data.IORef

-- | Variables of type 'FIORef a' are faceted 'IORef's
data FIORef a = FIORef (IORef (Faceted a))

-- | Allocate a new 'FIORef'
newFIORef :: Faceted a -> FIO (FIORef a)
newFIORef init = FIO newFIORefForPC
  where newFIORefForPC pc = do var <- newIORef (pcF pc init undefined)
                               return (FIORef var)

-- | Read an 'FIORef'
readFIORef :: FIORef a -> FIO (Faceted a)
readFIORef (FIORef var) = FIO readFIORefForPC
  where readFIORefForPC pc = do faceted <- readIORef var
                                return faceted 

-- | Write an 'FIORef'
writeFIORef :: FIORef a -> Faceted a -> FIO ()
writeFIORef (FIORef var) newValue = FIO writeFIORefForPC
  where writeFIORefForPC pc = do oldValue <- readIORef var
                                 writeIORef var (pcF pc newValue oldValue)

