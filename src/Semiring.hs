module Semiring where

import GHC.Base (liftA2)

class Semiring d where
    zero :: d
    one :: d
    plus :: d -> d -> d
    times :: d -> d -> d

class (Semiring d, Monoid e) => Module d e | e -> d where
    -- `d` is a scalar, and `e` is a vector type
    scalarMult :: d -> e -> e

class (Module d e, Semiring e) => Algebra d e where
    dagger :: d -> e
    dagger d = d `scalarMult` one

class Module d e => Kronecker v d e where
    delta :: v -> e

newtype SemiringAsAlgebra d = SA { unwrapSA :: d } deriving (Functor, Show)

instance Semiring d => Semigroup (SemiringAsAlgebra d) where
  (SA d) <> (SA d') = SA (d `plus` d')

instance Semiring d => Monoid (SemiringAsAlgebra d) where
  mempty = SA zero

instance Semiring d => Module d (SemiringAsAlgebra d) where
  d `scalarMult` (SA d') = SA (d `times` d')

instance Semiring d => Semiring (SemiringAsAlgebra d) where
  zero = SA zero
  one  = SA one
  (SA d) `plus`  (SA d') = SA (d `plus`  d')
  (SA d) `times` (SA d') = SA (d `times` d')

instance Num d => Num (SemiringAsAlgebra d) where
  SA lhs + SA rhs = SA $ lhs + rhs
  SA lhs * SA rhs = SA $ lhs * rhs
  negate = SA . negate . unwrapSA
  abs    = SA . abs    . unwrapSA
  signum = SA . signum . unwrapSA
  fromInteger = SA . fromInteger

instance Fractional d => Fractional (SemiringAsAlgebra d) where
  SA lhs / SA rhs = SA $ lhs / rhs
  fromRational = SA . fromRational

instance {-# OVERLAPPABLE #-} Semiring d => Algebra d (SemiringAsAlgebra d) where
  dagger = SA

instance Semiring Double where
  zero = 0
  one = 1
  plus = (+)
  times = (*)
