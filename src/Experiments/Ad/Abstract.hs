-- https://github.com/birthevdb/Forward-Reverse/blob/main/Abstract.hs

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}

module Experiments.Ad.Abstract where

import Experiments.Ad.Expressions

import Language.Haskell.TH
import Language.Haskell.TH.Syntax.Compat
import Data.Map (Map)

---------------
-- Semirings --
---------------

class Semiring d where
  zero   :: d
  one    :: d
  plus   :: d -> d -> d
  times  :: d -> d -> d

-- numeric semiring instance

instance {-# OVERLAPPABLE #-} Num a => Semiring a where
  zero  = 0
  {-# INLINE zero #-}
  one   = 1
  {-# INLINE one #-}
  plus  = (+)
  {-# INLINE plus #-}
  times = (*)
  {-# INLINE times #-}

instance Semiring a => Semiring (Splice Q a) where
  zero = [|| zero ||]
  {-# INLINE zero #-}
  one  = [|| one ||]
  {-# INLINE one #-}
  plus x y = [|| $$x `plus` $$y ||]
  {-# INLINE plus #-}
  times x y = [|| $$x `times` $$y ||]
  {-# INLINE times #-}

-- instance {-# OVERLAPPABLE #-} Num a => Semiring (Splice Q a) where
--   zero      = [|| 0 ||]
--   one       = [|| 1 ||]
--   plus x y  = [|| $$x + $$y ||]
--   times x y = [|| $$x * $$y ||]

-- instance Semiring Int where
--   zero   = 0
--   one    = 1
--   plus   = (+)
--   times  = (*)

-- instance Semiring (Splice Q Int) where
--   zero      = [|| 0 ||]
--   one       = [|| 1 ||]
--   plus x y  = [|| $$x + $$y ||]
--   times x y = [|| $$x * $$y ||]

-- the free semiring

instance Semiring (Expr v) where
  zero   = Zero
  one    = One
  plus   = Plus
  times  = Times

instance Semiring (Splice Q (Expr v)) where
  zero      = [|| Zero ||]
  one       = [|| One ||]
  plus x y  = [|| Plus $$x $$y ||]
  times x y = [|| Times $$x $$y ||]

-- eval (because a fold) witnesses initiality/freeness

eval :: Semiring d => (v -> d) -> Expr v -> d
eval gen (Var x)        = gen x
eval gen Zero           = zero
eval gen One            = one
eval gen (Plus   e1 e2) = eval gen e1 `plus`  eval gen e2
eval gen (Times  e1 e2) = eval gen e1 `times` eval gen e2
{-# INLINE eval #-}

---------------
-- d-modules --
---------------

-- if Module d e, then we say that e is a d-module

class (Semiring d, Monoid e) => Module d e | e -> d where
  sact :: d -> e -> e

----------------
-- d-algebras --
----------------

-- if e is d-module, and itself moreover a semiring,
-- then we speak of having a semiring algebra structure on e,
-- by analogy with rings/fields, and say that 'e is a d-algebra'

class (Module d e, Semiring e) => Algebra d e where
  shom :: d -> e
  shom = (`sact` one)
  {-# INLINE shom #-}

-- an important special case: a semiring d is always a d-algebra

newtype SemiringAsAlgebra d = SA { sa :: d } deriving (Functor, Show)

instance Semiring d => Semigroup (SemiringAsAlgebra d) where
  (SA d) <> (SA d') = SA (d `plus` d')
  {-# INLINE (<>) #-}

instance Semiring d => Monoid (SemiringAsAlgebra d) where
  mempty = SA zero
  {-# INLINE mempty #-}

instance Semiring d => Module d (SemiringAsAlgebra d) where
  d `sact` (SA d') = SA (d `times` d')
  {-# INLINE sact #-}

instance Semiring d => Semiring (SemiringAsAlgebra d) where
  zero = SA zero
  {-# INLINE zero #-}
  one  = SA one
  {-# INLINE one #-}
  (SA d) `plus`  (SA d') = SA (d `plus`  d')
  {-# INLINE plus #-}
  (SA d) `times` (SA d') = SA (d `times` d')
  {-# INLINE times #-}

instance {-# OVERLAPPABLE #-} Semiring d => Algebra d (SemiringAsAlgebra d) where
  shom = SA
  {-# INLINE shom #-}

-------------------
-- Clifford-Weil --
-------------------

data CliffordWeil d e = CW { dCW :: d, eCW :: e } deriving Show

instance Functor (CliffordWeil d) where
  fmap h (CW d e) = CW d (h e)
  {-# INLINE fmap #-}

-- the fundamental theorem: if e is a d-module, then CW d e is a semiring

instance Module d e => Semiring (CliffordWeil d e) where
  zero                        = CW zero mempty
  {-# INLINE zero #-}
  one                         = CW one  mempty
  {-# INLINE one #-}
  (CW f df) `plus`  (CW g dg) = CW (f `plus`  g) (df `mappend` dg)
  {-# INLINE plus #-}
  (CW f df) `times` (CW g dg) = CW (f `times` g) ((f `sact` dg) `mappend` (g `sact` df))
  {-# INLINE times #-}


-- instance {-# OVERLAPS #-} Module d e => Semiring (Splice Q (CliffordWeil d e)) where
--   zero = [|| CW zero mempty ||]
--   {-# INLINE zero #-}
--   one  = [|| CW one mempty ||]
--   {-# INLINE one #-}
--   plus e1 e2  = [|| let CW f df = $$e1
--                         CW g dg = $$e2
--                      in CW (f `plus` g) (df `mappend` dg) ||]
--   {-# INLINE plus #-}
--   times e1 e2 = [|| let CW f df = $$e1
--                         CW g dg = $$e2
--                      in CW (f `times` g) ((f `sact` dg) `mappend` (g `sact` df)) ||]
--   {-# INLINE times #-}

instance Module d e => Semigroup (CliffordWeil d e) where
  (<>) = plus
  {-# INLINE (<>) #-}

instance Module d e => Monoid (CliffordWeil d e) where
  mempty = zero
  {-# INLINE mempty #-}

instance Module d e => Module d (CliffordWeil d e) where
  d' `sact` (CW d e) = CW (d' `times` d) (d' `sact` e)
  {-# INLINE sact #-}

instance Module d e => Algebra d (CliffordWeil d e) where
  shom d = CW d mempty
  {-# INLINE shom #-}

---------------
-- Kronecker --
---------------

class Module d e => Kronecker v d e where
  delta      :: v -> e

-----------------
-- Abstract AD --
-----------------

abstractD :: Kronecker v d e => (v -> d) -> Expr v -> CliffordWeil d e
abstractD env = eval gen where gen v = CW (env v) (delta v)
{-# INLINE abstractD #-}

abstractDStaged ::
  ( Kronecker v d e,
    Kronecker (Splice Q v) (Splice Q d) (Splice Q e)
  ) =>
  (Splice Q v -> Splice Q d) ->
  Expr (Splice Q v) ->
  Splice Q (CliffordWeil d e)
abstractDStaged env = eval gen
  where
    gen v = [||CW $$(env v) $$(delta v)||]
{-# INLINE abstractDStaged #-}

-- with extraction function

abstractDExtract :: Kronecker v d e => (v -> d) -> Expr v -> e
abstractDExtract env = eCW . abstractD env
{-# INLINE abstractDExtract #-}

abstractDStagedExtract ::
  ( Kronecker v d e,
    Kronecker (Splice Q v) (Splice Q d) (Splice Q e)
  ) =>
  (Splice Q v -> Splice Q d) ->
  Expr (Splice Q v) ->
  Splice Q e
abstractDStagedExtract env exp = [|| eCW $$(abstractDStaged env exp) ||]
{-# INLINE abstractDStagedExtract #-}

type ExtractedStagedAD v d =
  (Ord v, Semiring d) =>
  (Splice Q v -> Splice Q d) ->
  Expr (Splice Q v) ->
  Splice Q (Map v d)
