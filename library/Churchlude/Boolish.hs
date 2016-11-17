module Churchlude.Boolish where

import Churchlude.External

class Boolish b where
  not :: b -> b
  (&&) :: b -> b -> b
  b && b' = not (not b || not b')
  (||) :: b -> b -> b
  b || b' = not (not b && not b')
  true :: b
  false :: b

instance (Boolish b) => Boolish (x -> b) where
  not f = not . f
  f && g = \x -> f x && g x
  true = const true
  false = const false
