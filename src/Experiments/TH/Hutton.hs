{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- https://mpickering.github.io/posts/2019-02-14-stage-3.html

module Experiments.TH.Hutton where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Compat
import Data.Functor.Identity

data Expr = Val Int | Add Expr Expr

eval1 :: Applicative m => Expr -> m Int
eval1 (Val n) = pure n
eval1 (Add e1 e2) = (+) <$> eval1 e1 <*> eval1 e2

eval2 :: Expr -> Splice Q Int
eval2 (Val n) = [||n||]
eval2 (Add e1 e2) = [||$$(eval2 e1) + $$(eval2 e2)||]

-- >>> $$(eval2 (Add (Val 1) (Val 2)))
-- 3

data WithSplice a = WithSplice
  { _val :: a,
    _Splice :: Splice Q a
  }

data SynApplicative a where
  Return :: WithSplice a -> SynApplicative a
  App    :: SynApplicative (a -> b) -> SynApplicative a -> SynApplicative b

-- liftT :: Lift a => a -> Splice Q a
-- liftT = unsafeTExpCoerce . lift

codePlus = [|| (+) ||]

elimExpr :: Expr -> Splice Q (SynApplicative Int)
elimExpr (Val n) = [|| Return (WithSplice n (liftTyped n)) ||]
elimExpr (Add e1 e2) =
    [|| Return (WithSplice (+) codePlus)
          `App` $$(elimExpr e1)
          `App` $$(elimExpr e2) ||]

data ApplicativeDict m =
  ApplicativeDict
    { _return :: forall a. WithSplice (a -> m a),
      _ap     :: forall a b. WithSplice (m (a -> b) -> m a -> m b)
    }

elimApplicative
  :: SynApplicative a
  -> ApplicativeDict m
  -> Splice Q (m a)

elimApplicative (Return v) d@ApplicativeDict{..}
  = [|| $$(_Splice _return) $$(_Splice v) ||]

elimApplicative (App e1 e2) d@ApplicativeDict{..}
  = [|| $$(_Splice _ap) $$(elimApplicative e1 d) $$(elimApplicative e2 d) ||]

idAp :: Identity (a -> b) -> Identity a -> Identity b
idAp (Identity f) (Identity a) = Identity (f a)

identityDict = ApplicativeDict{..}
  where
    _return = WithSplice Identity [|| Identity ||]
    _ap = WithSplice idAp [|| idAp ||]
