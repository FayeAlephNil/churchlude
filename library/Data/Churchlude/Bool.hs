module Data.Churchlude.Bool where

import Churchlude.External
import Churchlude.Eq
import Churchlude.Boolish

newtype Bool = Bool {
  -- Should pick one of these
  bool :: forall a . a -> a -> a
}

bool' :: a -> a -> Bool -> a
bool' a a' b = bool b a a'

-- TODO

instance Boolish Bool where
  not = bool' false true
  b && b' = bool b false (bool b' false true)
  true = Bool const
  false = Bool $ const id
  
instance Eq Bool where
  b == b' = bool b (bool b' true false) (bool b' false true)
