module SizedMat where

import GHC.TypeLits
import Data.Kind
import Data.Proxy
import qualified Numeric.LinearAlgebra.Static as LinAlg
import Numeric.LinearAlgebra.Static
import Numeric.LinearAlgebra (Transposable(..))

type Dual :: Nat -> Nat -> Nat -> Nat -> Type
data Dual n m n' m' = Dual { f :: L n' m' , df :: (L n' 1, L 1 m') -> L n m }

constMat :: forall n m. (KnownNat n, KnownNat m) => ℝ -> L n m
constMat v = build $ const $ const v

matmul :: forall m k n. (KnownNat m, KnownNat k, KnownNat n) => L m k -> L k n -> L m n
matmul = (LinAlg.<>)

{-
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
-}

ddotplus :: (KnownNat n, KnownNat m)
         => ((L n' l, L l m') -> L n m)
         -> ((L n' l, L l m') -> L n m)
         -> ((L n' l, L l m') -> L n m)
f `ddotplus` g = \pair -> f pair + g pair

actL :: (KnownNat n1, KnownNat n2, KnownNat l)
     => L n2 n1
     -> ((L n2 l, L l m1) -> L n m)
     -> ((L n1 l, L l m1) -> L n m)
x `actL` f = \(l, r) -> f (x `matmul` l, r)

-- actPointwise :: forall (d::Nat->Nat-> *) l n n1 m m1.(Num (d n1 l)) => d n1 l -> (d n1 l -> d l m1 -> d n m) -> (d n1 l -> d l m1 -> d n m)
-- x `actPointwise` f = \(l, r) -> f (x*l) r

actR :: (KnownNat m1, KnownNat l, KnownNat m2)
     => ((L n1 l, L l m2) -> L n m)
     -> L m1 m2
     -> ((L n1 l, L l m1) -> L n m)
f `actR` x = \(l, r) -> f (l, r `matmul` x)

{-
class MatSemiringStatic d where
    zero  :: (KnownNat n, KnownNat m) => d n m
    one   :: KnownNat n => d n n
    plus  :: (KnownNat n, KnownNat m) => d n m -> d n m -> d n m
    times :: (KnownNat l, KnownNat n, KnownNat m) => d n l -> d l m -> d n m
-}

-- Should this even exist? We could just define `times` in terms of pointwise multiplication (I
-- think) and use the standard commutative semiring. Then have a different operation that performs
-- the matrix multiplication.
type SizedSemiring :: (Nat -> Nat -> Type) -> Constraint
class SizedSemiring d where
    zero  :: (KnownNat n, KnownNat m) => d n m
    one   :: KnownNat n => d n n
    plus  :: (KnownNat n, KnownNat m) => d n m -> d n m -> d n m
    times :: (KnownNat n, KnownNat m, KnownNat k) => d n k -> d k m -> d n m

instance SizedSemiring L where
    zero = constMat 0
    one = eye
    plus = (+)
    times = matmul

type Expr :: (Nat -> Nat -> Type) -> Nat -> Nat -> Type
data Expr v n m where
    Var        :: v n m -> Expr v n m
    Zero       :: Expr v n m
    One        :: Expr v n n -- The identity matrix
    FromRat    :: Rational -> Expr v n m
    Negate     :: Expr v n m -> Expr v n m
    (:+:)      :: Expr v n m -> Expr v n m -> Expr v n m
    -- Matmul
    (:*:)      :: KnownNat k => Expr v n k -> Expr v k m -> Expr v n m
    -- Times      :: Expr v n m -> Expr v n m -> Expr v n m
    Transpose  :: Expr v m n -> Expr v n m
    (:/:)      :: Expr v n 1 -> Expr v n 1 -> Expr v n 1


instance (KnownNat n, KnownNat m) => Num (Expr v n m) where
    (+) = plus
    (*) = error "TODO"
    negate = Negate
    fromInteger  = FromRat . fromInteger
    abs = error "TODO"
    signum = error "TODO"

instance KnownNat n => Fractional (Expr v n 1) where
    (/) = (:/:)
    fromRational = FromRat

instance Transposable (Expr v n m) (Expr v m n) where
    tr  = Transpose
    tr' = Transpose

instance SizedSemiring (Expr v) where
    zero = Zero
    one = FromRat 1
    plus = (:+:)
    times = (:*:)

instance ( KnownNat n
         , KnownNat m
         , forall a b. (KnownNat a, KnownNat b) => Transposable (L a b) (L b a)
         ) => SizedSemiring (Dual n m) where
    zero = Dual zero $ const zero
    one  = Dual one  $ const zero
    Dual f df `plus`  Dual g dg = Dual (f `plus`  g) $ df `ddotplus` dg
    Dual f df `times` Dual g dg = Dual (f `times` g) $ (df `actR` tr g) `ddotplus` (tr f `actL` dg)

-- Requires a nightly version of GHC
evalSized :: ( SizedSemiring d
             , KnownNat n
             , KnownNat m
             , forall n' m'. (KnownNat n', KnownNat m') => Fractional   (d n' m')
             , forall n' m'. (KnownNat n', KnownNat m') => Transposable (d n' m') (d m' n')
             )
          => (forall n' m'. (KnownNat n', KnownNat m') => v n' m' -> d n' m')
          -> Expr v n m
          -> d n m
evalSized gen (Var x)       = gen x
evalSized gen Zero          = zero
evalSized gen One           = one
evalSized gen (FromRat f)   = fromRational f
evalSized gen (Negate e)    = negate (evalSized gen e)
evalSized gen (lhs :+: rhs) = evalSized gen lhs `plus` evalSized gen lhs
evalSized gen (lhs :*: rhs) = evalSized gen lhs `times` evalSized gen rhs
evalSized gen (lhs :/: rhs) = evalSized gen lhs / evalSized gen rhs
evalSized gen (Transpose e) = tr (evalSized gen e)
