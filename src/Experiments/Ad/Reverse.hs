{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}

module Experiments.Ad.Reverse where

import Data.Map
import Data.Array.IO
import Control.Monad (forM_)
import Control.Monad.State.Lazy
import Prelude hiding (map)
import Data.Array.Base (MArray(..))
import Control.Monad.Reader
import Language.Haskell.TH
import Language.Haskell.TH.Syntax.Compat

import Experiments.Ad.Expressions
import Experiments.Ad.Abstract
import Experiments.Ad.Forward

----------------------------------
-- Accumulating multiplications --
----------------------------------

newtype Hom d e = Hom { unHom :: d -> e }

reprHom :: Module d e => e -> Hom d e
reprHom e = Hom (\d -> d `sact` e)

absHom :: Module d e => Hom d e -> e
absHom (Hom f) = f one

-- instances

instance Semigroup e => Semigroup (Hom d e) where
  Hom f <> Hom g = Hom (\d -> f d <> g d)
  {-# INLINE (<>) #-}

instance Monoid e => Monoid (Hom  d e) where
  mempty = Hom (\d -> mempty)
  {-# INLINE mempty #-}

instance Module d e => Module d (Hom d e) where
  d' `sact` (Hom f) = Hom (\d -> f (d' `times` d))
  {-# INLINE sact #-}

-- generic instance

instance Kronecker v d e => Kronecker v d (Hom d e) where
  delta v = Hom (\d -> d `sact` delta v)
  {-# INLINE delta #-}

-- optimized instance

instance {-# OVERLAPS #-} (Ord v, Semiring d) => Kronecker v d (Hom d (SparseSA v d)) where
  delta v = Hom (\d -> Sparse (singleton v (SA d)))
  {-# INLINE delta #-}

-- reverse mode AD

reverseAD :: (Ord v, Semiring d) => (v -> d) -> Expr v -> CliffordWeil d (Hom d (SparseSA v d))
reverseAD = abstractD
{-# INLINE reverseAD #-}

instance Semigroup e => Semigroup (SpliceQ (Hom d e)) where
  (<>) e1 e2 = [|| let (Hom f) = $$e1
                       (Hom g) = $$e2
                    in Hom (\d -> f d <> g d) ||]
  {-# INLINE (<>) #-}

instance Monoid e => Monoid (SpliceQ (Hom d e)) where
  mempty = [|| Hom (\d -> mempty) ||]
  {-# INLINE mempty #-}

instance Module d e => Module (SpliceQ d) (SpliceQ (Hom d e)) where
  sact d' e2 = [|| let (Hom f) = $$e2
                    in Hom (\d -> f ($$d' `times` d)) ||]
  {-# INLINE sact #-}

instance Kronecker v d e => Kronecker (SpliceQ v) (SpliceQ d) (SpliceQ (Hom d e)) where
  delta v = [|| Hom (\d -> d `sact` delta $$v) ||]
  {-# INLINE delta #-}

instance {-# OVERLAPS #-} (Ord v, Semiring d) => Kronecker (SpliceQ v) (SpliceQ d) (SpliceQ (Hom d (SparseSA v d))) where
  delta v = [|| Hom (\d -> Sparse (singleton $$v (SA d))) ||]
  {-# INLINE delta #-}

reverseADStaged ::
  (Ord v, Semiring d) =>
  (SpliceQ v -> SpliceQ d) ->
  Expr (SpliceQ v) ->
  SpliceQ (CliffordWeil d (Hom d (SparseSA v d)))
reverseADStaged = abstractDStaged
{-# INLINE reverseADStaged #-}

-- with extraction function

reverseADExtract :: (Ord v, Semiring d) => (v -> d) -> Expr v -> Map v d
reverseADExtract gen = sparseSA . absHom . eCW . reverseAD gen
{-# INLINE reverseADExtract #-}

reverseADStagedExtract ::
  (Ord v, Semiring d) =>
  (SpliceQ v -> SpliceQ d) ->
  Expr (SpliceQ v) ->
  SpliceQ (Map v d)
reverseADStagedExtract env exp = [|| (sparseSA . absHom . eCW) $$(reverseADStaged env exp) ||]
{-# INLINE reverseADStagedExtract #-}

-- example

-- >>> reverseADExtract (\X -> 5) example3
-- fromList [(X,170)]

----------------------------
-- Accumulating additions --
----------------------------

newtype Endo e = E { unE :: e -> e }

reprEndo :: Monoid e => e -> Endo e
reprEndo e = E (\e' -> e' <> e)
{-# INLINE reprEndo #-}

absEndo :: Monoid e => Endo e -> e
absEndo (E f) = f mempty
{-# INLINE absEndo #-}

-- instances

instance Semigroup (Endo e) where
  E f <> E g = E (g . f)
  {-# INLINE (<>) #-}

instance Semigroup (SpliceQ (Endo e)) where
  (<>) e1 e2 = [|| let (E f) = $$e1
                       (E g) = $$e2
                    in E (g . f) ||]
  {-# INLINE (<>) #-}

instance Monoid (Endo e) where
  mempty = E id
  {-# INLINE mempty #-}

instance Monoid (SpliceQ (Endo e)) where
  mempty = [|| E id ||]
  {-# INLINE mempty #-}

instance Module d e => Module d (Endo e) where
  d `sact` E f = E (\e -> f (d `sact` e))
  {-# INLINE sact #-}

instance Module d e => Module (SpliceQ d) (SpliceQ (Endo e)) where
  d `sact` e2 = [|| let (E f) = $$e2
                     in E (\e -> f ($$d `sact` e)) ||]
  {-# INLINE sact #-}

-- optimized version

instance {-# OVERLAPS #-} (Ord v, Semiring d) => Kronecker v d (Hom d (Endo (Sparse v (SemiringAsAlgebra d)))) where
  delta v = Hom (\d -> E (\e -> Sparse (insertWith plus v (SA d) (sparse e))))
  {-# INLINE delta #-}

instance {-# OVERLAPS #-}
  (Ord v, Semiring d) =>
  Kronecker (SpliceQ v)
            (SpliceQ d)
            (SpliceQ (Hom d (Endo (Sparse v (SemiringAsAlgebra d))))) where
  delta v = [|| Hom (\d -> E (\e -> Sparse (insertWith plus $$v (SA d) (sparse e)))) ||]
  {-# INLINE delta #-}

reverseADEndo :: (Ord v, Semiring d) =>
  (v -> d) -> Expr v -> CliffordWeil d (Hom d (Endo (SparseSA v d)))
reverseADEndo = abstractD
{-# INLINE reverseADEndo #-}

reverseADEndoStaged ::
  (Ord v, Semiring d) =>
  (SpliceQ v -> SpliceQ d) ->
  Expr (SpliceQ v) ->
  SpliceQ (CliffordWeil d (Hom d (Endo (SparseSA v d))))
reverseADEndoStaged = abstractDStaged
{-# INLINE reverseADEndoStaged #-}

-- with extraction function

reverseADEndoExtract :: (Ord v, Semiring d) => (v -> d) -> Expr v -> Map v d
reverseADEndoExtract gen = sparseSA . absEndo . absHom . eCW . reverseADEndo gen
{-# INLINE reverseADEndoExtract #-}

reverseADEndoStagedExtract ::
  (Ord v, Semiring d) =>
  (SpliceQ v -> SpliceQ d) ->
  Expr (SpliceQ v) ->
  SpliceQ (Map v d)
reverseADEndoStagedExtract env exp = [|| (sparseSA . absEndo . absHom . eCW) $$(reverseADEndoStaged env exp) ||]
{-# INLINE reverseADEndoStagedExtract #-}

-- example

-- > reverseAD_Endo_extract (\X -> 5) example3
-- fromList [(X,170)]

--------------------
-- Mutable arrays --
--------------------

newtype SM d m = SM { sm :: m () }

-- instances

instance Monad m => Semigroup (SM d m) where
  SM com <> SM com' = SM (com >> com')
  {-# INLINE (<>) #-}

instance Monad m => Semigroup (SpliceQ (SM d m)) where
  e1 <> e2 = [|| let (SM com)  = $$e1 
                     (SM com') = $$e2 
                  in SM (com >> com') ||]
  {-# INLINE (<>) #-}

instance Monad m => Monoid (SM d m) where
  mempty = SM $ return ()
  {-# INLINE mempty #-}

instance Monad m => Monoid (SpliceQ (SM d m)) where
  mempty = [|| SM $ return () ||]
  {-# INLINE mempty #-}

-- reader monad combined with array monad

type MReadArray arr v e m = (Ix v, MArray arr e m, MonadReader (arr v e) m)

modifyArrayAt :: MReadArray arr v e m => (e -> e) -> v -> m ()
modifyArrayAt f v = do arr <- ask; a <- readArray arr v ; writeArray arr v (f a)
{-# INLINE modifyArrayAt #-}

-- instances

instance (Algebra d e, MReadArray arr v e m) => Module d (SM d m) where
  d `sact` com = SM $ do sm com; arr <- ask; b <- getBounds arr ; forM_ (range b) (modifyArrayAt (d `sact`))
  {-# INLINE sact #-}

instance (Algebra d e, MReadArray arr v e m) =>
  Module (SpliceQ d) (SpliceQ (SM d m)) where
  e1 `sact` e2 = [|| let d = $$e1
                         com = $$e2
                      in SM $ do
                         sm com
                         arr <- ask
                         b <- getBounds arr
                         forM_ (range b) (modifyArrayAt (d `sact`)) ||]
  {-# INLINE sact #-}

instance (Algebra d e, MReadArray arr v e m) => Kronecker v d (SM d m) where
  delta v = SM $ modifyArrayAt (`mappend` one) v
  {-# INLINE delta #-}

instance (Algebra d e, MReadArray arr v e m) =>
  Kronecker (SpliceQ v) (SpliceQ d) (SpliceQ (SM d m)) where
  delta v = [|| SM $ modifyArrayAt (`mappend` one) $$v ||]
  {-# INLINE delta #-}

instance {-# OVERLAPS #-} (Algebra d e, MReadArray arr v e m) => Kronecker v d (Hom d (SM d m)) where
  delta v = Hom (\d -> SM $ modifyArrayAt (`mappend` (shom d)) v)
  {-# INLINE delta #-}

instance {-# OVERLAPS #-} (Algebra d e, MReadArray arr v e m) =>
  Kronecker (SpliceQ v)
            (SpliceQ d)
            (SpliceQ (Hom d (SM d m))) where
  delta v = [|| Hom (\d -> SM $ modifyArrayAt (`mappend` (shom d)) $$v) ||]
  {-# INLINE delta #-}

-- reverseAD

reverseADArray :: (Algebra d e, MReadArray arr v e m)
                => (v -> d) -> Expr v -> CliffordWeil d (Hom d (SM d m))
reverseADArray = abstractD
{-# INLINE reverseADArray #-}

reverseADArrayStaged ::
  (Algebra d e, MReadArray arr v e m) =>
  (SpliceQ v -> SpliceQ d) ->
  Expr (SpliceQ v) ->
  SpliceQ (CliffordWeil d (Hom d (SM d m)))
reverseADArrayStaged = abstractDStaged
{-# INLINE reverseADArrayStaged #-}

-- with extraction functions

reverseADArrayExtract :: (Algebra d e, MReadArray arr v e m) => (v -> d) -> Expr v -> SM d m
reverseADArrayExtract gen = absHom . eCW . reverseADArray gen
{-# INLINE reverseADArrayExtract #-}

reverseADArrayStagedExtract ::
  (Algebra d e, MReadArray arr v e m) =>
  (SpliceQ v -> SpliceQ d) ->
  Expr (SpliceQ v) ->
  SpliceQ (SM d m)
reverseADArrayStagedExtract env exp = [|| (absHom . eCW) $$(reverseADArrayStaged env exp) ||]
{-# INLINE reverseADArrayStagedExtract #-}

-- for IO

reverseAD_CY_IO_extract :: forall v d. (Ix v, Semiring d) => (v -> d) -> Expr v -> (v,v) -> IO (Map v d)
reverseAD_CY_IO_extract gen e rng = do (arr :: IOArray v (SemiringAsAlgebra d)) <- newArray rng zero
                                       runReaderT (sm $ reverseADArrayExtract gen e) arr
                                       m <- getAssocs arr
                                       return $ map sa $ fromAscList m
{-# INLINE reverseAD_CY_IO_extract #-}

reverseAD_CY_IO_Staged_Extract ::
  forall v d. (Ix v, Semiring d) =>
  (SpliceQ v -> SpliceQ d) ->
  Expr (SpliceQ v) ->
  SpliceQ (v, v)
  -> SpliceQ (IO (Map v d))
reverseAD_CY_IO_Staged_Extract gen e rng = [|| do 
  (arr :: IOArray v (SemiringAsAlgebra d)) <- newArray $$rng zero
  runReaderT (sm $$(reverseADArrayStagedExtract gen e)) arr
  m <- getAssocs arr
  return $ map sa $ fromAscList m ||]
{-# INLINE reverseAD_CY_IO_Staged_Extract #-}

instance MArray arr e m => MArray arr e (ReaderT x m) where
   getBounds = lift . getBounds
   getNumElements = lift . getNumElements
   unsafeRead arr i = lift (unsafeRead arr i)
   unsafeWrite arr i v = lift (unsafeWrite arr i v)

-- example

-- > reverseAD_CY_IO_extract (\X -> 5) example3 (X,X)
-- fromList [(X,170)]
