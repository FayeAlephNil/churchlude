module Churchlude.Eq where

import Churchlude.Boolish
import Churchlude.External

class Eq a where
  (==) :: (Boolish b) => a -> a -> b
  a == b = not $ a /= b

  (/=) :: (Boolish b) => a -> a -> b
  a /= b = not $ a == b

class (Eq a) => Ord a where
  (<=) :: (Boolish b) => a -> a -> b
  n <= n' = (n < n') || (n == n')

  (>=) :: (Boolish b) => a -> a -> b
  (>=) = flip (<=)

  (<) :: (Boolish b) => a -> a -> b
  n < n' = (n <= n') && (n /= n')

  (>) :: (Boolish b) => a -> a -> b
  (>) = flip (<)

instance Eq () where
  () == () = true

-- instance Eq Int where
--   n == n' = runAsNats (f . left) (f . right) n
--     where
--       f m = either m negOption rightOption
--       negOption m = runAsNats (m ==) false n'
--       rightOption m = runAsNats false (m ==) n'
--
-- instance Ord Int where
--   n <= n' = runAsNats (f . left) (f . right) n
--     where
--       f m = either m negOption rightOption
--       negOption m = runAsNats (m <=) false n'
--       rightOption m = runAsNats false (m <=) n'
