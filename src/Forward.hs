module Forward where

import Data.Map

import Expression
import FnSpace
import Abstract
import Semiring

forwardAD :: forall v d. (Eq v, Fractional d, Semiring d) => (v -> d) -> Expr v -> v -> d
forwardAD gen = fmap unwrapSA . unwrapDense . ad
    where ad :: Expr v -> Dense v (SemiringAsAlgebra d)
          ad = df . abstractD gen

forwardSparseAD :: forall v d. (Ord v, Fractional d, Semiring d) => (v -> d) -> Expr v -> Map v d
forwardSparseAD gen = sparseSAToMap . ad
    where ad :: Expr v -> SparseSA v d
          ad = df . abstractD gen
