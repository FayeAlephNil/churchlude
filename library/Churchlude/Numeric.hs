module Churchlude.Numeric where

class Semigroup a where
  (+) :: a -> a -> a

class (Semigroup a) => Monoid a where
  zero :: a

class (Semigroup a) => Cancellative a where
  (-) :: a -> a -> a

class (Cancellative a, Monoid a) => Group a where
  invert :: a -> a
  invert = (zero -)

class (Monoid a) => Rg a where
  (*) :: a -> a -> a

type Rng a = (Cancellative a, Rg a)

class (Rg a) => Rig a where
  one :: a

type Ring a = (Rng a, Rig a)
