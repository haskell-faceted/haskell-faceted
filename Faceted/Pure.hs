{-# LANGUAGE GADTs,RankNTypes,DeriveFunctor #-}

module Faceted.Pure (
  Label,
  Faceted,
  makePrivate,
  makeFaceted,
  makePublic,
  bottom
  ) where

import Faceted.Internal

import Control.Applicative

bottom = Raw undefined

-- | < k ? x : bottom >   ====>  makePrivate k x

makePrivate :: Label -> a -> Faceted a
makePrivate k x = Faceted k (Raw x) (bottom)

-- | x ==> Raw x ===> makePublic x

makePublic :: a -> Faceted a
makePublic x = Raw x

makeFaceted = Faceted

