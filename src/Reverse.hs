module Reverse (reverseEndoAD, reverseEndoMapAD) where

import GHC.Base (liftA2)
import Data.Map

import FnSpace
import Expression
import Semiring
import Abstract

newtype Hom d e = Hom { unwrapHom :: d -> e }

reprHom :: Module d e => e -> Hom d e
reprHom e = Hom $ \d -> d `scalarMult` e

absHom :: Module d e => Hom d e -> e
absHom (Hom f) = f one

instance Semigroup e => Semigroup (Hom d e) where
  Hom lhs <> Hom rhs = Hom $ \d -> lhs d <> rhs d

instance Monoid e => Monoid (Hom d e) where
  mempty = Hom $ \d -> mempty

instance Module d e => Module d (Hom d e) where
  d' `scalarMult` (Hom f) = Hom $ \d -> f (d' `times` d)

instance (Num e, Monoid e, Semigroup e) => Num (Hom d e) where
  (+) = (<>)
  -- is this correct???
  Hom lhs * Hom rhs = Hom $ \d -> lhs d * rhs d
  negate = Hom . fmap negate . unwrapHom
  abs    = Hom . fmap abs    . unwrapHom
  signum = Hom . fmap signum . unwrapHom

  fromInteger 0 = mempty
  -- Have no ideia what this should be
  fromInteger _ = undefined

instance (Ord v, Semiring d) => Kronecker v d (Hom d (SparseSA v d)) where
  delta v = Hom $ \d -> Sparse $ singleton v (SA d)

-- instance Kronecker v d e => Kronecker v d (Hom d e) where
--   delta v = Hom $ \d -> d `scalarMult` delta v

-- reverseAD :: forall v d. (Eq v, Semiring d) => (v -> d) -> Expr v -> v -> d
-- reverseAD gen = fmap unwrapSA . unwrapDense . absHom . ad
--     where ad :: Expr v -> Hom d (Dense v (SemiringAsAlgebra d))
--           ad = df . abstractD gen

-- reverseSparseAD :: forall v d. (Ord v, Semiring d) => (v -> d) -> Expr v -> Map v d
-- reverseSparseAD gen = sparseSA . absHom . ad
--   where ad :: Expr v -> Hom d (SparseSA v d)
--         ad = df . abstractD gen

newtype Endo e = Endo { unwrapEndo :: e -> e }

reprEndo :: Monoid e => e -> Endo e
reprEndo e = Endo $ \e' -> e' <> e

absEndo :: Monoid e => Endo e -> e
absEndo (Endo f) = f mempty

instance Semigroup (Endo e) where
  Endo f <> Endo g = Endo $ g . f

instance Monoid (Endo e) where
  mempty = Endo id

instance Module d e => Module d (Endo e) where
  d `scalarMult` Endo f = Endo (\e -> f (d `scalarMult` e))

instance (Num e, Monoid e, Semigroup e) => Num (Endo e) where
  (+) = (<>)
  -- is this correct???
  Endo lhs * Endo rhs = Endo $ liftA2 (*) lhs rhs
  negate = Endo . fmap negate . unwrapEndo
  abs    = Endo . fmap abs    . unwrapEndo
  signum = Endo . fmap signum . unwrapEndo

  fromInteger 0 = mempty
  -- No idea what this should be
  fromInteger _ = undefined

-- instance (Ord v, Semiring d) => Kronecker v d (Hom d (Endo (SparseSA v d))) where
--   delta v = Hom $ \d -> Endo $ \e -> Sparse $ insertWith plus v (SA d) (unwrapSparse e)

instance (Ord v, Semiring d) => Kronecker v d (Hom d (Endo (SparseSA v d))) where
  delta v = Hom $ \d -> Endo $ \e -> Sparse $ insertWith plus v (SA d) (unwrapSparse e)

reverseEndoMapAD :: forall v d. (Ord v, Fractional d, Semiring d) => (v -> d) -> Expr v -> Map v d
reverseEndoMapAD gen expr = sparseSAToMap $ absEndo $ absHom $ ad expr
  where ad :: Expr v -> Hom d (Endo (SparseSA v d))
        ad = df . abstractD gen

reverseEndoAD :: forall v d. (Ord v, Fractional d, Semiring d) => (v -> d) -> Expr v -> v -> d
reverseEndoAD gen expr var = maybe zero unwrapSA $
                                Data.Map.lookup var $
                                  unwrapSparse $ absEndo $ absHom $ ad expr
  where ad :: Expr v -> Hom d (Endo (SparseSA v d))
        ad = df . abstractD gen
