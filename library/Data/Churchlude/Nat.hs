module Data.Churchlude.Nat where

import Churchlude.External
import Data.Churchlude.Bool
import Churchlude.Numeric
import Churchlude.Eq
import Churchlude.Boolish

data Nat = Nat {
  iter :: forall a . (a -> a) -> a -> a
}

isZero :: (Boolish b) => Nat -> b
isZero m = iter m (const false) true

succ :: Nat -> Nat
succ n = Nat $ \f a -> f (iter n f a)

pred :: Nat -> Nat
pred n = Nat $ \f x -> iter n (wrap f) (const x) id
  where
    wrap f g h = h (g f)

recurseNat :: Nat -> a -> (Nat -> a -> a) -> a
recurseNat n base step = bool (equalZero n) base $ recurseNat (pred n) (step n base) step
  where
    equalZero x = iter x (const false) true

instance Eq Nat where
  n == n' = (n <= n') && (n' <= n)

instance Ord Nat where
  n <= n' = isZero (n - n')

instance Semigroup Nat where
  n + n' = iter n' succ n

instance Cancellative Nat where
  n - n' = iter n' pred n

instance Monoid Nat where
  zero = Nat $ \_ a -> a

instance Rg Nat where
  n * n' = iter n' (n +) zero

instance Rig Nat where
  one = succ zero
