{-# LANGUAGE MultiParamTypeClasses #-}

module Subtype where

import Data.Churchlude

class a <: b where
  cast :: a -> b

instance Nat <: Int where
  cast n = int n 0
