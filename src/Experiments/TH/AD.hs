{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Use tuple-section" #-}

module Experiments.TH.AD where

import Language.Haskell.TH

-------------------------------------------------------
-- 2.1 Symbolic Expressions and their Evaluation
-------------------------------------------------------

data Expr v
  = Var v
  | Zero
  | One
  | Add (Expr v) (Expr v)
  | Mul (Expr v) (Expr v)
  deriving Show

data X = X
  deriving (Show, Eq)

example1 :: Expr X
example1 = Mul (Var X) (Add (Var X) One)

class Semiring d where
  zero :: d
  one  :: d
  add  :: d -> d -> d
  mul  :: d -> d -> d

instance Semiring (Expr v) where
  zero = Zero
  one  = One
  add  = Add
  mul  = Mul

instance {-# Overlappable #-} Num a => Semiring a where
  zero = 0
  one  = 1
  add  = (+)
  mul  = (*)

eval :: Semiring d => (v -> d) -> Expr v -> d
eval var (Var x) = var x
eval var Zero    = zero
eval var One     = one
eval var (Add e1 e2) = eval var e1 `add` eval var e2
eval var (Mul e1 e2) = eval var e1 `mul` eval var e2

-- >>> eval (\x -> 5) example1 :: Int
-- 30

-- >>> eval (const One) example1 :: Expr Int
-- Mul One (Add One One)

type Dual d = (d, d)

instance Semiring d => Semiring (Dual d) where
  zero = (zero, zero)
  one  = (one,  zero)
  (f, df) `add` (g, dg) = (f `add` g, df `add` dg)
  (f, df) `mul` (g, dg) = (f `mul` g, (g `mul` df) `add` (f `mul` dg))

forwardAD :: (Eq v, Semiring d) => (v -> d) -> v -> Expr v -> Dual d
forwardAD var x = eval gen
  where
    gen y = (var y, partialDerive x y)
    partialDerive x y = if x == y then one else zero

-- >>> example1
-- Mul (Var X) (Add (Var X) One) -- x * (x + 1)

-- >>> forwardAD (\x -> One) X example1 :: (Expr Int, Expr Int)
-- (Mul One (Add One One),Add (Mul (Add One One) One) (Mul One (Add One Zero)))
-- (1 * (1 + 1), ((1 + 1) * 1) + (1 * (1 + 0)))

-- >>> forwardAD (\x -> 5) X example1 :: (Int, Int)
-- (30,11)
-- (5 * (5 + 1), ((5 + 1) * 1) + (1 * (5 + 1)))

-- Still not what we want!
-- >>> (x, dx) = forwardAD (\x -> [|| 5 ||]) X example1 :: (Code Q Int, Code Q Int)
-- >>> ($$x, $$dx)
-- (30,11)

instance Semiring d => Semiring (Code Q d) where
  zero = [|| zero ||]
  one  = [|| one ||]
  add x y = [|| $$x `add` $$y ||]
  mul x y = [|| $$x `mul` $$y ||]

-- instance Semiring d => Semiring (Code Q (Dual d)) where
--   zero = [|| (zero, zero) ||]
--   one  = [|| (one,  zero) ||]
--   add e1 e2 = [|| let (f, df) = $$e1
--                       (g, dg) = $$e2
--                    in (f `add` g, df `add` dg) ||]
--   mul e1 e2 = [|| let (f, df) = $$e1
--                       (g, dg) = $$e2
--                    in (f `mul` g, (g `mul` df) `add` (f `mul` dg)) ||]

forwardADStaged ::
  (Eq v, Semiring d) =>
  (Code Q v -> Code Q d) ->
  Code Q v ->
  Expr (Code Q v) ->
  Code Q (Dual d)
forwardADStaged var x = eval gen
  where
    gen y = [|| ($$(var y), $$(partialDerive x y)) ||]
    partialDerive x y = [|| if $$x == $$y then one else zero ||]

example2 :: Expr (Code Q X)
example2 = Mul (Var [||X||]) (Add (Var [||X||]) One)

-- >>> result = forwardADStaged (\x -> [||5||]) [||X||] example2 :: Code Q (Int, Int)
-- >>> $$(result)
-- Variable not in scope: example2 :: Expr (Code Q X)

-------------------------------------------------------
-- 2.2 Algebraic Structures
-------------------------------------------------------

class (Semiring d, Monoid e) => Module d e | e -> d
  where
    (•) :: d -> e -> e

class (Module d e, Semiring e) => Algebra d e
  where
    (†) :: d -> e
    (†) d = d • one

-- a semiring d is always a d-algebra
newtype SemiringAsAlgebra d = SA { sa :: d } deriving (Functor, Show)

instance Semiring d => Semigroup (SemiringAsAlgebra d) where
  (SA d) <> (SA d') = SA (d `add` d')

instance Semiring d => Monoid (SemiringAsAlgebra d) where
  mempty = SA zero

instance Semiring d => Module d (SemiringAsAlgebra d) where
  d • (SA d') = SA (d `mul` d')

-- instance (Semiring d, Monoid d) => Module d d
--   where (•) = mul

-- instance (Semiring d, Monoid d) => Algebra d d
--   where (†) = id

-- Clifford-Weil
data CliffordWeil d e = CW { dCW :: d, eCW :: e }

instance Module d e => Semiring (CliffordWeil d e) where
  zero = CW zero mempty
  one  = CW one  mempty
  (CW f df) `add` (CW g dg) = CW (f `add` g) (df `mappend` dg)
  (CW f df) `mul` (CW g dg) = CW (f `mul` g) ((f • dg) `mappend` (g • df))

instance Module d e => Semigroup (CliffordWeil d e) where
  (<>) = add

instance Module d e => Monoid (CliffordWeil d e) where
  mempty = zero

instance Module d e => Module d (CliffordWeil d e) where
  d' • (CW d e) = CW (d' `mul` d) (d' • e)

class Module d e => Kronecker v d e where
  delta :: v -> e

-- Abstract Differentiation
abstractD :: Kronecker v d e => (v -> d) -> Expr v -> CliffordWeil d e
abstractD gen = eval env where env x = CW (gen x) (delta x)

-- ================================================= --
--  3 Forward-Mode Automatic Differentiation
-- ================================================= --

-------------------------------------------------------
-- 3.1 Forward-Mode AD as AD in the Dense Function Space
-------------------------------------------------------

type Dense v e = v -> e

instance Module d e => Module d (Dense v e) where
  d • f = \v -> d • f v

instance (Eq v, Algebra d e) => Kronecker v d (Dense v e) where
  delta v = \w -> if v == w then one else zero

-- forwardAD' :: (Eq v, Semiring d) => (v -> d) -> Expr v -> CliffordWeil d (Dense v (SemiringAsAlgebra d))
-- forwardAD' = abstractD