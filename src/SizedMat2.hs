{-# LANGUAGE PolyKinds, FlexibleContexts #-}

module SizedMat2 where

import GHC.TypeLits
import Data.Kind
import Data.Proxy
import qualified Numeric.LinearAlgebra.Static as LinAlg
-- import Numeric.LinearAlgebra.Static
import Numeric.LinearAlgebra (Transposable(..))
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Map (DMap)
import Data.Functor.Identity (Identity(..))
import Data.Dependent.Sum
import Data.GADT.Compare
import Data.Type.Equality
import Data.Function (on)

-- ** Functions on sized matricies **

constMat :: forall n m. (KnownNat n, KnownNat m) => LinAlg.ℝ -> LinAlg.L n m
constMat v = LinAlg.build $ const $ const v

matmul :: forall m k n. (KnownNat m, KnownNat k, KnownNat n)
       => LinAlg.L m k
       -> LinAlg.L k n
       -> LinAlg.L m n
matmul = (LinAlg.<>)

-- ** Typeclasses **

type SizedSemiring :: (Nat -> Nat -> Type) -> Constraint
class SizedSemiring d where
    zero  :: (KnownNat n, KnownNat m) => d n m
    one   :: KnownNat n => d n n
    plus  :: (KnownNat n, KnownNat m) => d n m -> d n m -> d n m
    times :: (KnownNat n, KnownNat m, KnownNat k) => d n k -> d k m -> d n m

type SizedModule :: (Nat -> Nat -> Type) -> Type -> Constraint
class (SizedSemiring d, Monoid e) => SizedModule d e | e -> d where
    -- `d` is a scalar, and `e` is a vector type
    scalarMult :: d n m -> e -> e


-- class (Module d e, Semiring e) => Algebra d e where
--     dagger :: d -> e
--     dagger d = d `scalarMult` one
-- 

-- type Kronecker :: Type -> (Nat -> Nat -> Type) -> Type -> Constraint
type SizedKronecker :: (k -> Type) -> (Nat -> Nat -> Type) -> Type -> Constraint
class SizedModule d e => SizedKronecker v d e where
    delta :: v k -> e

-- ** Instances for L **

instance SizedSemiring LinAlg.L where
  zero = constMat 0
  one = LinAlg.eye
  plus = (+)
  times = matmul

instance (KnownNat n, KnownNat m) => Semigroup (LinAlg.L n m) where
  (<>) = (+)

instance (KnownNat n, KnownNat m) => Monoid (LinAlg.L n m) where
  mempty = constMat 0

-- FIXME: This is not always possible.
instance (KnownNat n, KnownNat m) => SizedModule LinAlg.L (LinAlg.L n m) where
  lhs `scalarMult` rhs = undefined

-- ** Instances for Expr **

type Expr :: ((Nat, Nat) -> Type) -> Nat -> Nat -> Type
data Expr v n m where
    Var        :: v '(n, m) -> Expr v n m
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

deriving instance (forall n m. Show (v '(n, m))) => Show (Expr v n m)

instance (KnownNat n, KnownNat m) => Num (Expr v n m) where
    (+) = plus
    negate = Negate
    fromInteger  = FromRat . fromInteger
    (*) = error "TODO"
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
    one = One
    plus = (:+:)
    times = (:*:)

-- ** Instances for Dual **

type Dual :: (Nat -> Nat -> Type) -- A 'matrix' type that takes 2 dimensions
          -> Type
          -> Nat -> Nat           -- A pair of dimensions for `f`
          -> Type
data Dual d e n m = Dual { f :: d n m, df :: e }


instance ( SizedSemiring d
         , SizedModule d e
         , forall a b. (KnownNat a, KnownNat b) => Transposable (d a b) (d b a)
         ) => SizedSemiring (Dual d e) where
    zero = Dual zero mempty
    one  = Dual one  mempty
    Dual f df `plus`  Dual g dg = Dual (f `plus`  g) $ (df <> dg)
    Dual f df `times` Dual g dg = Dual (f `times` g) $ ((g `scalarMult` df) <> (f `scalarMult` dg))

instance ( SizedSemiring d
         , KnownNat n
         , KnownNat m
         , Num (d n m)
         , forall a b. (KnownNat a, KnownNat b) => Transposable (d a b) (d b a)
         , Num e
         , SizedModule d e
         ) => Num (Dual d e n m) where
    (+) = plus
    negate (Dual f df)  = Dual (negate f) $ negate df
    fromInteger i = Dual (fromInteger i) mempty
    (*) = error "TODO"
    abs = error "TODO"
    signum = error "TODO"

instance ( SizedSemiring d
         , KnownNat n
         , Fractional (d n 1)
         , forall a b. (KnownNat a, KnownNat b) => Transposable (d a b) (d b a)
         , Num e
         , SizedModule d e
         ) => Fractional (Dual d e n 1) where
    (Dual f df) / (Dual g dg) = Dual (f / g) $
      (recip (g ^ 2)) `scalarMult` ((g `scalarMult` df) - (f `scalarMult` dg))
    fromRational = error "TODO"


instance ( KnownNat n
         , KnownNat m
         , forall a b. (KnownNat a, KnownNat b) => Transposable (d a b) (d b a)
         ) => Transposable (Dual d e n m) (Dual d e m n) where
    -- FIXME: VERY LIKELY TO BE WRONG
    tr (Dual f df) = Dual (tr f) df
    tr' = tr

-- ** Instances for Mat **

type Mat :: (Nat, Nat) -> Type
data Mat a where
    Wrap :: LinAlg.L n m -> Mat '(n, m)

deriving instance (KnownNat n, KnownNat m) => Show (Mat '(n, m))

unMat :: (KnownNat n, KnownNat m) => Mat '(n, m) -> LinAlg.L n m
unMat (Wrap mat) = mat

instance (KnownNat n, KnownNat m) => Num (Mat '(n, m)) where
  (+) = fmap Wrap . (+) `on` unMat
  (*) = fmap Wrap . (*) `on` unMat
  abs = Wrap . abs . unMat
  signum = Wrap . signum . unMat
  fromInteger = Wrap . fromInteger
  negate = Wrap . negate . unMat

-- instance SizedSemiring Mat where
--     zero = constMat 0
--     one = LinAlg.eye
--     plus = (+)
--     times = matmul

-- ** Instances for SizedSparse **

-- type Sparse :: (k -> Type) -> (k -> Type) -> Type
-- newtype Sparse k f = Sparse { unwrapSparse :: DMap k f }

data Sparse = Sparse { unwrapSparse :: DMap Var Mat }

instance Semigroup Sparse where
  Sparse lhs <> Sparse rhs = Sparse $
    DMap.unionWithKey f lhs rhs
      where f :: forall v. Var v -> Mat v -> Mat v -> Mat v
            f TagX l r = Wrap $ unMat l + unMat r
            f TagY l r = Wrap $ unMat l + unMat r

instance Monoid Sparse where
  mempty = Sparse DMap.empty

instance Num Sparse where
  (+) = (<>)
  negate = Sparse . DMap.mapWithKey f . unwrapSparse
    where f :: forall v. Var v -> Mat v -> Mat v
          f TagX = Wrap . negate . unMat
          f TagY = Wrap . negate . unMat

  abs    = Sparse . DMap.mapWithKey f . unwrapSparse
    where f :: forall v. Var v -> Mat v -> Mat v
          f TagX = Wrap . abs . unMat
          f TagY = Wrap . abs . unMat

  signum = Sparse . DMap.mapWithKey f . unwrapSparse
    where f :: forall v. Var v -> Mat v -> Mat v
          f TagX = Wrap . signum . unMat
          f TagY = Wrap . signum . unMat

  fromInteger = const $ Sparse mempty
  (*) = undefined

instance SizedModule LinAlg.L Sparse where
  d `scalarMult` (Sparse m) = Sparse $ DMap.mapWithKey f m
    where f :: forall v. Var v -> Mat v -> Mat v
          f TagX = Wrap . (d `scalarMult`) . unMat
          f TagY = Wrap . (d `scalarMult`) . unMat

{-
-- SizedAlgebra d e
instance SizedModule d Sparse => SizedKronecker Var d Sparse where
  delta v = Sparse $ singleton v one
-}

-- ** evaluation **

-- Requires a nightly version of GHC
evalSized :: ( SizedSemiring d
             , KnownNat n
             , KnownNat m
             , forall n'. KnownNat n' => Fractional (d n' 1)
             , forall n' m'. (KnownNat n', KnownNat m') => Num (d n' m')
             , forall n' m'. (KnownNat n', KnownNat m') => Transposable (d n' m') (d m' n')
             )
          => (forall n' m'. (KnownNat n', KnownNat m') => v '(n', m') -> d n' m')
          -> Expr v n m
          -> d n m
evalSized gen (Var x)       = gen x
evalSized gen Zero          = zero
evalSized gen One           = one
evalSized gen (FromRat f)   = error "TODO"
evalSized gen (Negate e)    = negate (evalSized gen e)
evalSized gen (lhs :+: rhs) = evalSized gen lhs `plus` evalSized gen lhs
evalSized gen (lhs :*: rhs) = evalSized gen lhs `times` evalSized gen rhs
evalSized gen (lhs :/: rhs) = evalSized gen lhs / evalSized gen rhs
evalSized gen (Transpose e) = tr (evalSized gen e)

type Var :: (Nat, Nat) -> Type
data Var a where
    TagX :: Var '(4, 3)
    TagY :: Var '(3, 2)

deriving instance Show (Var a)

instance GEq Var where
    geq TagX TagX = Just Refl
    geq TagY TagY = Just Refl
    geq _ _ = Nothing

instance GCompare Var where
    lhs `gcompare` rhs
      | Just Refl <- geq lhs rhs = GEQ
      | otherwise = case lhs of
                      TagX -> GLT
                      TagY -> GGT

forwardSparseAD :: ( KnownNat n
                   , KnownNat m
                   , forall n' m'. KnownNat n' => Fractional (Dual LinAlg.L Sparse n' 1)
                   , forall n' m'. (KnownNat n', KnownNat m') => Transposable (LinAlg.L n' m') (LinAlg.L m' n')
                   )
                => (forall n' m'. (KnownNat n', KnownNat m') => Var '(n', m') -> LinAlg.L n' m')
                -> Expr Var n m
                -> DMap Var Mat
forwardSparseAD gen = unwrapSparse . df . evalSized env
  where env :: forall n' m'. (KnownNat n', KnownNat m') => Var '(n', m') -> Dual LinAlg.L Sparse n' m'
        env x = Dual (gen x) (Sparse $ DMap.singleton x (Wrap $ constMat 1))

expr = Var TagX :*: Var TagY

values :: forall n m. (KnownNat n, KnownNat m) => Var '(n, m) -> LinAlg.L n m
values TagX = LinAlg.fromList [  1,  2,  3
                              ,  4,  5,  6
                              ,  7,  8,  9
                              , 10, 11, 12 ]
values TagY = LinAlg.fromList [ 1, 2
                              , 4, 5
                              , 7, 8 ]

-- 1. Figure out what sizes are involved in the matrix multiplications
-- 2. What kind of dimensions we need to store in the map

{-
-- abstractD :: (Fractional d, Num e, Kronecker v d e) => (v -> d) -> Expr v -> Dual d e
abstractD :: ( SizedSemiring d
             , KnownNat n
             , KnownNat m
             , Fractional (d n m)
             , Num e
             )
          => (forall n' m'. Var '(n', m') -> L n' m')
          -> Expr Var n m
          -> Dual n m 1 1
abstractD gen = evalSized env
    where env :: forall n' m'. (KnownNat n', KnownNat m') => Var '(n', m') -> DMap n' m'
          env x = Dual (gen x) (singleton x)

forwardSparseAD :: forall v d
                 . ( Ord v
                   , forall n' m'. (KnownNat n', KnownNat m') => Fractional (d n' m')
                   , forall n' m'. (KnownNat n', KnownNat m') => Transpose  (d n' m') (d m' n')
                   , SizedSemiring d
                   )
                => (v -> d)
                -> Expr v
                -> Map v d
forwardSparseAD gen = sparseSAToMap . ad
    where ad :: Expr v -> SparseSA v d
          ad = df . abstractD gen

reverseADStatic :: forall d n m.(IndexedSemiringStatic d, KnownNat n, KnownNat m,forall a.KnownNat a=> Norm (d a 1), forall a b.(KnownNat a, KnownNat b)=>Floating (d a b), forall a b.(KnownNat a, KnownNat b)=>Transposable (d a b) (d b a)) => (forall n1 m1.(KnownNat n1, KnownNat m1) => NetVar n1 m1 -> d n1 m1) -> NetVar n m -> IndexedExprStatic 1 1 -> StaticDualR d n m 1 1
reverseADStatic env W1 e = evalStatic gen e

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
-}
