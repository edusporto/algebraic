module FnSpace where

import Prelude hiding (map)
import Data.Map

import Semiring
import Abstract

newtype Sparse v e = Sparse { unwrapSparse :: Map v e }

deriving instance (Ord v, Show v, Show e) => Show (Sparse v e)

instance (Ord v, Module d e) => Semigroup (Sparse v e) where
  Sparse lhs <> Sparse rhs = Sparse $ unionWith (<>) lhs rhs

instance (Ord v, Module d e) => Monoid (Sparse v e) where
  mempty = Sparse empty

instance (Ord v, Module d e) => Module d (Sparse v e) where
  d `scalarMult` (Sparse m) = Sparse $ map (d `scalarMult`) m

instance (Ord v, Algebra d e) => Kronecker v d (Sparse v e) where
  delta v = Sparse $ singleton v one

instance (Ord v, Num e) => Num (Sparse v e) where
  -- is this correct???
  Sparse lhs + Sparse rhs = Sparse $ unionWith (+) lhs rhs
  Sparse lhs * Sparse rhs = Sparse $ unionWith (*) lhs rhs
  negate = Sparse . fmap negate . unwrapSparse
  abs    = Sparse . fmap abs    . unwrapSparse
  signum = Sparse . fmap signum . unwrapSparse
  fromInteger = const $ Sparse mempty

type SparseSA v d = Sparse v (SemiringAsAlgebra d)

sparseSAToMap :: SparseSA v d -> Map v d
sparseSAToMap = map unwrapSA . unwrapSparse

newtype Dense d e = Dense { unwrapDense :: d -> e }
    deriving Semigroup via (d -> e)
    deriving Monoid via (d -> e)

instance Module d e => Module d (Dense v e) where
    d `scalarMult` (Dense f) = Dense $ \v -> d `scalarMult` (f v)

instance (Eq v, Algebra d e) => Kronecker v d (Dense v e) where
    delta v = Dense $ \w -> if v == w then one else zero

instance (Eq v, Num e) => Num (Dense v e) where
  -- is this correct???
  Dense lhs + Dense rhs = Dense $ \d -> lhs d + rhs d
  Dense lhs * Dense rhs = Dense $ \d -> lhs d * rhs d
  negate = Dense . fmap negate . unwrapDense
  abs    = Dense . fmap abs    . unwrapDense
  signum = Dense . fmap signum . unwrapDense
  fromInteger = undefined
