{-# LANGUAGE GADTs,RankNTypes,DeriveFunctor #-}

module Faceted.FIO (
  FIO,
  secureRunFIO,
  swap
) where

import Faceted.Internal

-- | With an empty context it is safe to run
secureRunFIO fio = runFIO fio []

-- | Commuting pure and effectful facets
-- Optimized to prune inconsistent views
swap :: Faceted (FIO a) -> FIO (Faceted a)
swap (Raw fio) = FIO (\pc -> do runFIO fio pc >>= (return . Raw))
swap (Faceted k priv pub) = FIO swapForPC
  where swapForPC pc
          | Private k `elem` pc = runFIO (swap priv) pc
          | Public k  `elem` pc = runFIO (swap pub) pc
          | otherwise           = do privV <- runFIO (swap priv) (Private k : pc)
                                     pubV  <- runFIO (swap pub)  (Public k : pc)
                                     return (Faceted k privV pubV)
swap Bottom = FIO (\_ -> return Bottom)
