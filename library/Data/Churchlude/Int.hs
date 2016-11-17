module Data.Churchlude.Int (
int,
Int
) where

import Data.Churchlude.Nat
import Data.Churchlude.Tuple
import Data.Churchlude.Bool
import Churchlude.External
import Churchlude.Numeric
import Churchlude.Eq

newtype Int = Int (Nat :*: Nat)

int :: Nat -> Nat -> Int
int n n' = Int $ canon (n # n')
  where
    canon t = bool (isZero $ fst t) t $
                bool (isZero $ snd t) t $
                  canon $ pred (fst t) # pred (snd t)

instance Eq Int where
  (Int t) == (Int t') = (fst t + snd t') == (snd t + fst t')

instance Semigroup Int where
  (Int t) + (Int t') = Int $ (fst t + fst t') # (snd t + snd t')

instance Cancellative Int where
  (Int t) - (Int t') = Int $ (fst t + snd t') # (snd t + fst t')
