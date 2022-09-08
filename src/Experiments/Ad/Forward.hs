{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Experiments.Ad.Forward where

import Data.Map
import Prelude hiding (map)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax.Compat

import Experiments.Ad.Expressions
import Experiments.Ad.Abstract

-------------------------------
-- Classical forward-mode AD --
-------------------------------

type ClassicalDual d = CliffordWeil d (SemiringAsAlgebra d)

fstD :: Semiring d => ClassicalDual d -> d
fstD = dCW

sndD :: Semiring d => ClassicalDual d -> d
sndD = sa . eCW

forwardAD :: (Eq v, Semiring d) => (v -> d) -> Expr v -> v -> d
forwardAD env exp v = sndD $ eval gen exp where
  gen w = CW (env w) (k w)
  k   w = if v == w then one else zero

forwardADStaged ::
  (Eq v, Semiring d) =>
  (SpliceQ v -> SpliceQ d) ->
  Expr (SpliceQ v) ->
  SpliceQ v ->
  SpliceQ d
forwardADStaged env exp x = [|| sndD $$(eval gen exp) ||] where
  gen y = [|| CW $$(env y) $$(partialDerive x y) ||]
  partialDerive x y = [|| if $$x == $$y then one else zero ||]

-- example

-- < forwardAD (\X -> 5) example1 X
-- 11

--------------------------
-- Dense function space --
--------------------------

type Dense v e = v -> e

-- instances

instance Module d e => Module d (Dense v e) where
  d `sact` (f) = h where h v = d `sact` (f v)

instance (Eq v, Algebra d e) => Kronecker v d (Dense v e) where
  delta v = \ w -> if v == w then one else zero

instance Semigroup e => (Semigroup (SpliceQ (Dense v e))) where
  (<>) e1 e2 = [|| let f = $$e1
                       g = $$e2
                    in \d -> f d <> g d ||]

instance Monoid e => (Monoid (SpliceQ (Dense v e))) where
  mempty = [|| const mempty ||]

instance Module d e => Module (SpliceQ d) (SpliceQ (Dense v e)) where
  sact e1 e2 = [|| let d = $$e1
                       f = $$e2
                    in \v -> d `sact` (f v) ||]

instance (Eq v, Algebra d e) =>
  Kronecker
    (SpliceQ v)
    (SpliceQ d)
    (SpliceQ (Dense v e)) where
  delta v = [|| \w -> if $$v == w then one else zero ||]

-- forward AD

abstractforwardAD :: (Eq v, Semiring d) => (v -> d) -> Expr v -> CliffordWeil d (Dense v (SemiringAsAlgebra d))
abstractforwardAD = abstractD
-- abstractforwardAD gen e = sa . (abstractD_extract gen e)

abstractforwardADExtract :: (Eq v, Semiring d) => (v -> d) -> Expr v -> v -> d
abstractforwardADExtract gen = fmap sa . eCW . abstractforwardAD gen

abstractforwardADStaged ::
  (Eq v, Semiring d) =>
  (SpliceQ v -> SpliceQ d) ->
  Expr (SpliceQ v) ->
  SpliceQ (CliffordWeil d (Dense v (SemiringAsAlgebra d)))
abstractforwardADStaged = abstractDStaged

abstractforwardADStagedExtract ::
  (Eq v, Semiring d) =>
  (SpliceQ v -> SpliceQ d) ->
  Expr (SpliceQ v) ->
  SpliceQ v ->
  SpliceQ d
abstractforwardADStagedExtract gen exp x = [|| ((fmap sa . eCW) $$(abstractforwardADStaged gen exp)) $$x ||]

-- example

-- < abstractforwardAD_extract (\X -> 5) example1 X
-- 11

---------------
-- Gradients --
---------------

-- inefficient version

type AllDual v d = Dense v (ClassicalDual d)

forwardGradient :: (Eq v, Semiring d) => (v -> d) -> Expr v -> CliffordWeil d (AllDual v d)
forwardGradient = abstractD

forwardGradientStaged ::
  (Eq v, Semiring d) =>
  (SpliceQ v -> SpliceQ d) ->
  Expr (SpliceQ v) ->
  SpliceQ (CliffordWeil d (AllDual v d))
forwardGradientStaged = abstractDStaged

-- with extraction function

forwardGradientExtract :: (Eq v, Semiring d) => (v -> d) -> Expr v -> v -> d
forwardGradientExtract gen = fmap dCW . eCW . forwardGradient gen

forwardGradientStagedExtract ::
  (Eq v, Semiring d) =>
  (SpliceQ v -> SpliceQ d) ->
  Expr (SpliceQ v) ->
  SpliceQ v ->
  SpliceQ d
forwardGradientStagedExtract gen exp x = [|| (fmap dCW . eCW) $$(forwardGradientStaged gen exp) $$x ||] 

-- example

-- > let e = let {env X1 = 5; env X2 = 3} in forwardGradient_extract env example2 in (e X1, e X2)
-- (4, 5)

-- optimized version

type DenseSA v d = Dense v (SemiringAsAlgebra d)

abstractSharedGradient :: (Eq v, Semiring d) => (v -> d) -> Expr v -> CliffordWeil d (DenseSA v d)
abstractSharedGradient = abstractD

abstractSharedGradientStaged ::
  (Eq v, Semiring d) =>
  (SpliceQ v -> SpliceQ d) ->
  Expr (SpliceQ v) ->
  SpliceQ (CliffordWeil d (DenseSA v d))
abstractSharedGradientStaged = abstractDStaged

-- with extraction function

abstractSharedGradientExtract :: (Eq v, Semiring d) => (v -> d) -> Expr v -> v -> d
abstractSharedGradientExtract gen = fmap sa . eCW . abstractSharedGradient gen

abstractSharedGradientStagedExtract ::
  (Eq v, Semiring d) =>
  (SpliceQ v -> SpliceQ d) ->
  Expr (SpliceQ v) ->
  SpliceQ v ->
  SpliceQ d
abstractSharedGradientStagedExtract gen exp x = [||(fmap sa . eCW) $$(abstractSharedGradientStaged gen exp) $$x||]
-- example

-- < let e = let {env X1 = 5; env X2 = 3} in abstractSharedGradient_extract env example2 in (e X1, e X2)
-- (4, 5)

-----------------
-- Sparse maps --
-----------------

newtype Sparse v e = Sparse { sparse :: Map v e }

deriving instance (Ord v, Show v, Show e) => Show (Sparse v e)

sparseToDense :: forall v e. (Ord v, Monoid e) => Sparse v e -> Dense v e
sparseToDense (Sparse m) = f where f v = findWithDefault (mempty :: e) v m

-- instances

instance (Ord v, Module d e) => Semigroup (Sparse v e) where
  Sparse f <> Sparse g = Sparse (unionWith (<>) f g)

instance (Ord v, Module d e) => Semigroup (SpliceQ (Sparse v e)) where
  (<>) e1 e2 = [|| let (Sparse f) = $$e1
                       (Sparse g) = $$e2
                    in Sparse (unionWith (<>) f g) ||]

instance (Ord v, Module d e) => Monoid (Sparse v e) where
  mempty = Sparse empty

instance (Ord v, Module d e) => Monoid (SpliceQ (Sparse v e)) where
  mempty = [|| Sparse empty ||]

instance (Ord v, Module d e) => Module d (Sparse v e) where
  d `sact` (Sparse m) = Sparse $ map (d `sact`) m

instance (Ord v, Module d e) => Module (SpliceQ d) (SpliceQ (Sparse v e)) where
  sact d e2 = [|| let (Sparse m) = $$e2
                   in Sparse $ map ($$d `sact`) m ||]

instance (Ord v, Algebra d e) => Kronecker v d (Sparse v e) where
  delta v = Sparse $ singleton v one

instance (Ord v, Algebra d e) =>
  Kronecker (SpliceQ v)
            (SpliceQ d)
            (SpliceQ (Sparse v e)) where
  delta v = [||Sparse (singleton $$v one)||]

-- this representation allows to write the following Sparse form of gradient computation

type SparseSA v d = Sparse v (SemiringAsAlgebra d)

sparseSA :: SparseSA v d -> Map v d
sparseSA = map sa . sparse

-- forward AD

forwardADSparse :: (Ord v, Semiring d) => (v -> d) -> Expr v -> CliffordWeil d (SparseSA v d)
forwardADSparse = abstractD

forwardADSparseStaged ::
  (Ord v, Semiring d) =>
  (SpliceQ v -> SpliceQ d) ->
  Expr (SpliceQ v) ->
  SpliceQ (CliffordWeil d (SparseSA v d))
forwardADSparseStaged = abstractDStaged

-- with extraction function

forwardADSparseExtract :: (Ord v, Semiring d) => (v -> d) -> Expr v -> Map v d
forwardADSparseExtract gen = sparseSA . eCW . forwardADSparse gen

forwardADSparseStagedExtract ::
  (Ord v, Semiring d) =>
  (SpliceQ v -> SpliceQ d) ->
  Expr (SpliceQ v) ->
  SpliceQ (Map v d)
forwardADSparseStagedExtract gen exp = [|| (sparseSA . eCW) $$(forwardADSparseStaged gen exp) ||]

-- example

-- > let {env X1 = 5; env X2 = 3} in forwardADSparse_extract env example2
-- fromList [(X1,4),(X2,5)]
