{-# LANGUAGE GADTs,RankNTypes,DeriveFunctor #-}

module Faceted.FHandle (
  View,
  FHandle,
  openFileF,
  hPutCharF,
  hGetCharF,
  hCloseF,
  hPutStrF,
  ) where

import Faceted.Internal

import System.IO

import Control.Monad(liftM, join)

import Faceted.FIO(swap)

-- | Facet-aware file handles
data FHandle = FHandle View Handle
 
openFileF :: View -> FilePath -> IOMode -> FIO FHandle
openFileF view path mode = FIO $ \_ ->
  do putStrLn "opening a file"
     handle <- openFile path mode
     return (FHandle view handle)

hCloseF :: FHandle -> FIO ()
hCloseF (FHandle _ handle) = FIO $ \_ -> hClose handle

hGetCharF :: FHandle -> FIO (Faceted Char)
hGetCharF (FHandle view handle) = FIO hGetCharForPC
  where hGetCharForPC pc
          | pc `visibleTo` view = do
              ch <- hGetChar handle
              return (pcF (map Private view) (Raw ch) Bottom)
          | otherwise = return Bottom

hPutCharF :: FHandle -> Faceted Char -> FIO ()
hPutCharF (FHandle view handle) ch = FIO f where
  f :: PC -> IO ()
  f pc | pc `visibleTo` view = case project view (runFaceted ch pc) of
                                 Just c -> do
                                   putStrLn $ "printing " ++ show c
                                   hPutChar handle c
                                 Nothing -> return ()
       | otherwise           = return ()

prod = liftM join . swap

-- For convenience
hPutStrF :: FHandle -> Faceted String -> FIO ()
hPutStrF h fs = do
  prod $ do
    s <- fs
    return $ liftM return $ sequence $ map (hPutCharF h . return) s
  return ()
