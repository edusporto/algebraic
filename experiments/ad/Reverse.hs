{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}

module Reverse where

import Data.Map
import Data.Array.IO
import Control.Monad (forM_)
import Control.Monad.State.Lazy
import Prelude hiding (map)
import Data.Array.Base (MArray(..))
import Control.Monad.Reader
import Language.Haskell.TH

import Expressions
import Abstract
import Forward

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

instance Monoid e => Monoid (Hom  d e) where
  mempty = Hom (\d -> mempty)

instance Module d e => Module d (Hom d e) where
  d' `sact` (Hom f) = Hom (\d -> f (d' `times` d))

-- generic instance

instance Kronecker v d e => Kronecker v d (Hom d e) where
  delta v = Hom (\d -> d `sact` delta v)

-- optimized instance

instance (Ord v, Semiring d) => Kronecker v d (Hom d (SparseSA v d)) where
  delta v = Hom (\d -> Sparse (singleton v (SA d)))

-- reverse mode AD

reverseAD :: (Ord v, Semiring d) => (v -> d) -> Expr v -> CliffordWeil d (Hom d (SparseSA v d))
reverseAD = abstractD

instance Semigroup e => Semigroup (TExpQ (Hom d e)) where
  (<>) e1 e2 = [|| let (Hom f) = $$e1
                       (Hom g) = $$e2
                    in Hom (\d -> f d <> g d) ||]
  
instance Monoid e => Monoid (TExpQ (Hom d e)) where
  mempty = [|| Hom (\d -> mempty) ||]

instance Module d e => Module (TExpQ d) (TExpQ (Hom d e)) where
  sact d' e2 = [|| let (Hom f) = $$e2
                    in Hom (\d -> f ($$d' `times` d)) ||]

instance Kronecker v d e => Kronecker (TExpQ v) (TExpQ d) (TExpQ (Hom d e)) where
  delta v = [|| Hom (\d -> d `sact` delta $$v) ||]

instance (Ord v, Semiring d) => Kronecker (TExpQ v) (TExpQ d) (TExpQ (Hom d (SparseSA v d))) where
  delta v = [|| Hom (\d -> Sparse (singleton $$v (SA d))) ||]

reverseADStaged ::
  (Ord v, Semiring d) =>
  (TExpQ v -> TExpQ d) ->
  Expr (TExpQ v) ->
  TExpQ (CliffordWeil d (Hom d (SparseSA v d)))
reverseADStaged = abstractDStaged

-- with extraction function

reverseADExtract :: (Ord v, Semiring d) => (v -> d) -> Expr v -> Map v d
reverseADExtract gen = sparseSA . absHom . eCW . reverseAD gen

reverseADStagedExtract ::
  (Ord v, Semiring d) =>
  (TExpQ v -> TExpQ d) ->
  Expr (TExpQ v) ->
  TExpQ (Map v d)
reverseADStagedExtract env exp = [|| sparseSA $ absHom $ eCW $$(reverseADStaged env exp) ||]

-- example

-- >>> reverseADExtract (\X -> 5) example3
-- fromList [(X,170)]

----------------------------
-- Accumulating additions --
----------------------------

newtype Endo e = E { unE :: e -> e }

reprEndo :: Monoid e => e -> Endo e
reprEndo e = E (\e' -> e' <> e)

absEndo :: Monoid e => Endo e -> e
absEndo (E f) = f mempty

-- instances

instance Semigroup (Endo e) where
  E f <> E g = E (g . f)

instance Monoid (Endo e) where
  mempty = E id

instance Module d e => Module d (Endo e) where
  d `sact` E f = E (\e -> f (d `sact` e))

-- optimized version

instance (Ord v, Semiring d) => Kronecker v d (Hom d (Endo (Sparse v (SemiringAsAlgebra d)))) where
  delta v = Hom (\d -> E (\e -> Sparse (insertWith plus v (SA d) (sparse e))))

reverseADEndo :: (Ord v, Semiring d) =>
  (v -> d) -> Expr v -> CliffordWeil d (Hom d (Endo (SparseSA v d)))
reverseADEndo = abstractD

-- with extraction function

reverseADEndoExtract :: (Ord v, Semiring d) => (v -> d) -> Expr v -> Map v d
reverseADEndoExtract gen = sparseSA . absEndo . absHom . eCW . reverseADEndo gen

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

instance Monad m => Monoid (SM d m) where
  mempty = SM $ return ()

-- reader monad combined with array monad

type MReadArray arr v e m = (Ix v, MArray arr e m, MonadReader (arr v e) m)

modifyArrayAt :: MReadArray arr v e m => (e -> e) -> v -> m ()
modifyArrayAt f v = do arr <- ask; a <- readArray arr v ; writeArray arr v (f a)

-- instances

instance (Algebra d e, MReadArray arr v e m) => Module d (SM d m) where
  d `sact` com = SM $ do sm com; arr <- ask; b <- getBounds arr ; forM_ (range b) (modifyArrayAt (d `sact`))

instance (Algebra d e, MReadArray arr v e m) => Kronecker v d (SM d m) where
  delta v = SM $ modifyArrayAt (`mappend` one) v

instance (Algebra d e, MReadArray arr v e m) => Kronecker v d (Hom d (SM d m)) where
  delta v = Hom (\d -> SM $ modifyArrayAt (`mappend` (shom d)) v)

-- reverseAD

reverseADArray :: (Algebra d e, MReadArray arr v e m)
                => (v -> d) -> Expr v -> CliffordWeil d (Hom d (SM d m))
reverseADArray = abstractD

-- with extraction functions

reverseADArrayExtract  :: (Algebra d e, MReadArray arr v e m) => (v -> d) -> Expr v -> SM d m
reverseADArrayExtract gen = absHom . eCW . reverseADArray gen

-- for IO

reverseAD_CY_IO_extract :: forall v d. (Ix v, Semiring d) => (v -> d) -> Expr v -> (v,v) -> IO (Map v d)
reverseAD_CY_IO_extract gen e rng = do (arr :: IOArray v (SemiringAsAlgebra d)) <- newArray rng zero
                                       runReaderT (sm $ reverseADArrayExtract gen e) arr
                                       m <- getAssocs arr
                                       return $ map sa $ fromAscList m

instance MArray arr e m => MArray arr e (ReaderT x m) where
   getBounds = lift . getBounds
   getNumElements = lift . getNumElements
   unsafeRead arr i = lift (unsafeRead arr i)
   unsafeWrite arr i v = lift (unsafeWrite arr i v)

-- example

-- > reverseAD_CY_IO_extract (\X -> 5) example3 (X,X)
-- fromList [(X,170)]
