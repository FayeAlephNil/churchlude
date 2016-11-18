module Data.Churchlude.Int (
int,
Int,
splitInt,
abs
) where

import Data.Churchlude.Nat
import Data.Churchlude.Tuple
import Data.Churchlude.Either
import Data.Churchlude.Bool
import Churchlude.External
import Churchlude.Numeric
import Churchlude.Eq
import Churchlude.Subtype

newtype Int = Int (Nat :*: Nat)

canon :: Nat :*: Nat -> Either Nat Nat
canon t = let
  ft = fst t
  st = snd t
  in bool (isZero ft) (left st) $
            bool (isZero st) (right ft) $
              canon $ pred ft # pred st

canon' :: Nat :*: Nat -> Nat :*: Nat
canon' = canon >>> either' (zero #) (# zero)

int :: Nat -> Nat -> Int
int n n' = Int $ canon' (n # n')

-- left ~ negative
-- right ~ positive
splitInt :: Int -> Either Nat Nat
splitInt (Int t) = canon t

abs :: Int -> Nat
abs = splitInt >>> either' id id

instance Nat <: Int where
  cast n = int n zero

instance Eq Int where
  (Int t) == (Int t') = (fst t + snd t') == (snd t + fst t')

instance Ord Int where
  (Int t) <= (Int t') = (fst t + snd t') <= (snd t + fst t')

instance Semigroup Int where
  (Int t) + (Int t') = Int $ (fst t + fst t') # (snd t + snd t')

instance Cancellative Int where
  (Int t) - (Int t') = Int $ (fst t + snd t') # (snd t + fst t')

instance Monoid Int where
  zero = int zero zero

instance Group Int where
  invert (Int t) = Int $ snd t # fst t

instance Rg Int where
  (Int t) * (Int t') = Int $ (a * n + m * b) # (b * n + m * a)
    where
      a = fst t
      b = snd t
      n = fst t'
      m = snd t'

instance Rig Int where
  one = int one zero
