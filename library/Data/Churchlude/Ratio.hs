module Data.Churchlude.Ratio (Ratio) where

import Data.Churchlude.Int
import Data.Churchlude.Nat
import Churchlude.Subtype
import Churchlude.Eq
import Data.Churchlude.Tuple
import Churchlude.Numeric
import Churchlude.External

newtype Ratio = Ratio (Int :*: Int)

-- TODO gcd
-- ratio :: Int -> Int -> Maybe Ratio
-- ratio i i' = _

instance Int <: Ratio where
  cast i = Ratio $ i # one

instance Nat <: Ratio where
  cast = (cast :: Nat -> Int) >>> cast

instance Eq Ratio where
  (Ratio t) == (Ratio t') = (fst t * snd t') == (snd t * fst t')

instance Ord Ratio where
  (Ratio t) <= (Ratio t') = (fst t * snd t') <= (snd t * fst t')

instance Semigroup Ratio where
  (Ratio t) + (Ratio t') = Ratio $ (ft * st' + ft' * st) # (st * st')
    where
      ft  = fst t
      st  = snd t
      ft' = fst t'
      st' = snd t'

instance Cancellative Ratio where
  r - r' = r + invert r'

instance Monoid Ratio where
  zero = Ratio $ zero # one

instance Group Ratio where
  invert (Ratio t) = Ratio $ invert (fst t) # snd t

instance Rg Ratio where
  (Ratio t) * (Ratio t') = Ratio $ (fst t * fst t') # (snd t * snd t')

instance Rig Ratio where
  one = Ratio $ one # one
