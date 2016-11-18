module Data.Churchlude.Either where

import Churchlude.External
import Churchlude.Eq
import Churchlude.Boolish

newtype Either a b = Either {
  either :: forall r . (a -> r) -> (b -> r) -> r
}

type a :+: b = Either a b

either' :: (a -> r) -> (b -> r) -> Either a b -> r
either' f g e = either e f g

left :: a -> Either a b
left a = Either $ \f _ -> f a

right :: b -> Either a b
right b = Either $ \_ g -> g b

eitherMap :: (b -> Either a c) -> Either a b -> Either a c
eitherMap = either' left

instance Functor (Either a) where
  fmap f = (>>= (right . f))

instance Applicative (Either a) where
  pure = right
  fs <*> as = fs >>= (`fmap` as)

instance Monad (Either a) where
  e >>= f = eitherMap f e

instance (Eq a, Eq b) => Eq (Either a b) where
  e == e' = either e leftOption rightOption
    where
      leftOption  l = either e' (== l) (const false)
      rightOption r = either e' (const false) (== r)
