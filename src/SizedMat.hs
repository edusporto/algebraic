module SizedMat where

import GHC.TypeLits
import Data.Kind
import Data.Proxy
import qualified Numeric.LinearAlgebra.Static as LinAlg
import Numeric.LinearAlgebra.Static

import Semiring

type Dual :: Nat -> Nat -> Nat -> Nat -> Type
data Dual n m n' m' = Dual { f :: L n' m' , df :: (L n' 1, L 1 m') -> L n m }

constMat :: forall n m. (KnownNat n, KnownNat m) => ℝ -> L n m
constMat v = build $ const $ const v

matmul :: forall m k n. (KnownNat m, KnownNat k, KnownNat n) => L m k -> L k n -> L m n
matmul = (LinAlg.<>)

ddotplus :: (KnownNat n, KnownNat m)
         => ((L n' l, L l m') -> L n m)
         -> ((L n' l, L l m') -> L n m)
         -> ((L n' l, L l m') -> L n m)
f `ddotplus` g = \pair -> f pair + g pair

actL :: (KnownNat n, KnownNat n', KnownNat l)
     => L n' n
     -> ((L n' l, L l m') -> L n m)
     -> ((L n  l, L l m') -> L n m)
x `actL` f = \(lhs, rhs) -> f (x `matmul` lhs, rhs)

actR :: (KnownNat m, KnownNat l, KnownNat m')
     => ((L n' l, L l m') -> L n m)
     -> L m m'
     -> ((L n' l, L l m) -> L n m)
f `actR` x = \(lhs, rhs) -> f (lhs, rhs `matmul` x)

actPointwise :: forall l n n' m m'. (KnownNat l, KnownNat n')
             => L n' l
             -> ((L n' l, L l m') -> L n m)
             -> ((L n' l, L l m') -> L n m)
x `actPointwise` f = \(lhs, rhs) -> f (x * lhs, rhs)

{-
class MatSemiringStatic d where
    zero  :: (KnownNat n, KnownNat m) => d n m
    one   :: KnownNat n => d n n
    plus  :: (KnownNat n, KnownNat m) => d n m -> d n m -> d n m
    times :: (KnownNat l, KnownNat n, KnownNat m) => d n l -> d l m -> d n m
-}

-- type SizedSemiring :: (Nat -> Nat -> Type) -> Constraint
-- class SizedSemiring d where
--     zero  :: (KnownNat n, KnownNat m) => d n m
--     one   :: KnownNat n => d n m
--     plus  :: (KnownNat n, KnownNat m) => d n m -> d n m -> d n m
--     times :: (KnownNat n, KnownNat m, KnownNat k) => d n k -> d k m -> d n m

data Mat m where
    _

instance (KnownNat n, KnownNat m, KnownNat n', KnownNat m') => Semiring (Dual n m n' m') where
    zero = Dual zero $ const zero
    one = Dual one $ const one
    Dual f df `plus`  Dual g dg = Dual (f `plus`  g) (df `ddotplus` dg)
    Dual f df `times` Dual g dg = Dual (f `times` g) $ \(lhs, rhs) -> undefined
    -- Dual f df `times` Dual g dg = Dual (f `times` g) ((df `actR` tr g) `ddotplus` (tr f `actL` dg))
