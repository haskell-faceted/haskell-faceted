{-# LANGUAGE GADTs,RankNTypes,DeriveFunctor #-}

module Faceted.FIO (
  FIO,
  runFIO,
  swap,
  primitive
) where

import Faceted.Internal

import Control.Monad(liftM)

-- | With an empty context it is safe to run
secureRunFIO fio = runFIO fio []

prod :: Faceted (FIO (Faceted a)) -> FIO (Faceted a)
prod ua = FIO f where
  f pc = g (runFaceted ua pc) where
    g (Raw fio) = runFIO fio pc
    g (Faceted k priv pub)
        | Private k `elem` pc = runFIO (prod priv) pc
        | Public k  `elem` pc = runFIO (prod pub) pc
        | otherwise           = do privV <- runFIO (prod priv) (Private k : pc)
                                   pubV  <- runFIO (prod pub)  (Public k : pc)
                                   return (Faceted k privV pubV)
    g Bottom = return Bottom

primitive :: FIO Int
primitive = FIO $ \pc ->
  let result | Private "H" `elem` pc = return 42
             | Public  "H" `elem` pc = return (-1)
             | otherwise             = return (-1)
  in result

swap :: Faceted (FIO a) -> FIO (Faceted a)
swap = prod . liftM (liftM return)
