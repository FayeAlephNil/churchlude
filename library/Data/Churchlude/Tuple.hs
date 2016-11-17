module Data.Churchlude.Tuple where

import Churchlude.External
import Churchlude.Boolish
import Churchlude.Eq

data Tuple a b = Tuple {
  tuple :: forall r . (a -> b -> r) -> r
}

type a :*: b = Tuple a b

(#) :: a -> b -> a :*: b
a # b = Tuple $ \f -> f a b

fst :: a :*: b -> a
fst t = tuple t const

snd :: a :*: b -> b
snd t = tuple t $ const id

instance Functor (Tuple a) where
  fmap f t = Tuple $ \g -> g (fst t) (f . snd $ t)

instance (Eq a, Eq b) => Eq (Tuple a b) where
  t == t' = tuple t tupleFunc
    where
      tupleFunc a b = tuple t' $ \a' b' -> (a == a') && (b == b')

-- instance (Monoid a) => Applicative (Tuple a) where
--   pure b = mempty # b
