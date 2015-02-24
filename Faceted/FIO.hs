{-# LANGUAGE GADTs,RankNTypes,DeriveFunctor #-}

module Faceted.FIO (
  FIO,
  secureRunFIO,
  branch
) where

import Faceted.Internal

-- | With an empty context it is safe to run
secureRunFIO fio = runFIO fio []

-- | Commuting pure and effectful facets
-- Optimized to prune inconsistent views
branch :: Faceted (FIO a) -> FIO (Faceted a)
branch (Raw fio)   = FIO (\pc -> do runFIO fio pc >>= (return . Raw))
branch (Faceted k priv pub) = FIO branchForPC
  where branchForPC pc
          | Private k `elem` pc = runFIO (branch priv) pc
          | Public k  `elem` pc = runFIO (branch pub) pc
          | otherwise           = do privV <- runFIO (branch priv) (Private k : pc)
                                     pubV  <- runFIO (branch pub)  (Public k : pc)
                                     return (Faceted k privV pubV)
branch Bottom = FIO (\_ -> return Bottom)
