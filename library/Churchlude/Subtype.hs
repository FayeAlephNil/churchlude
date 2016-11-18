module Churchlude.Subtype where

class a <: b where
  cast :: a -> b
