{-# LANGUAGE GADTs,RankNTypes,DeriveFunctor #-}

module Faceted.FHandle (
  View,
  FHandle,
  openFileF,
  hPutCharF,
  hGetCharF,
  hCloseF,
  ) where

import Faceted.Internal

import System.IO

import Faceted.FIO(swap)

-- | Facet-aware file handles
data FHandle = FHandle View Handle
 
openFileF :: View -> FilePath -> IOMode -> FIO FHandle
openFileF view path mode = FIO $ \_ ->
  do handle <- openFile path mode
     return (FHandle view handle)

hCloseF :: FHandle -> FIO ()
hCloseF (FHandle _ handle) = FIO $ \_ -> hClose handle

hGetCharF :: FHandle -> FIO (Faceted Char)
hGetCharF (FHandle view handle) = FIO hGetCharForPC
  where hGetCharForPC pc =
          do ch <- hGetChar handle
             return (pcF (map Private view) (Raw ch) (Raw undefined))

hPutCharF :: FHandle -> Faceted Char -> FIO ()
hPutCharF (FHandle view handle) ch = FIO hPutCharForPC
  where hPutCharForPC pc | pc `visibleTo` view = hPutChar handle (project view ch)
                         | otherwise = return ()

-- For convenience
hPutStrF :: FHandle -> Faceted String -> FIO (Faceted ())
hPutStrF h fs = swap $ do
  s <- fs
  return $ do
    sequence $ map (hPutCharF h . return) s
    return ()
