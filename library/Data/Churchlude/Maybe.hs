module Data.Churchlude.Maybe where

import Data.Churchlude.Either
import Churchlude.External

type Maybe a = Either () a

nothing :: Maybe a
nothing = left ()

just :: a -> Maybe a
just = right

maybe :: Maybe a -> r -> (a -> r) -> r
maybe m r = either m (const r)

maybe' :: r -> (a -> r) -> Maybe a -> r
maybe' r = either' (const r)
