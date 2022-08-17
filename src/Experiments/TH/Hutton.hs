{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- https://mpickering.github.io/posts/2019-02-14-stage-3.html

module Experiments.TH.Hutton where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Functor.Identity

data Expr = Val Int | Add Expr Expr

eval1 :: Applicative m => Expr -> m Int
eval1 (Val n) = pure n
eval1 (Add e1 e2) = (+) <$> eval1 e1 <*> eval1 e2

eval2 :: Expr -> Code Q Int
eval2 (Val n) = [||n||]
eval2 (Add e1 e2) = [||$$(eval2 e1) + $$(eval2 e2)||]

-- >>> $$(eval2 (Add (Val 1) (Val 2)))
-- 3

data WithCode a = WithCode
  { _val :: a,
    _code :: Code Q a
  }

data SynApplicative a where
  Return :: WithCode a -> SynApplicative a
  App    :: SynApplicative (a -> b) -> SynApplicative a -> SynApplicative b

-- liftT :: Lift a => a -> Code Q a
-- liftT = unsafeTExpCoerce . lift

codePlus = [|| (+) ||]

elimExpr :: Expr -> Code Q (SynApplicative Int)
elimExpr (Val n) = [|| Return (WithCode n (liftTyped n)) ||]
elimExpr (Add e1 e2) =
    [|| Return (WithCode (+) codePlus)
          `App` $$(elimExpr e1)
          `App` $$(elimExpr e2) ||]

data ApplicativeDict m =
  ApplicativeDict
    { _return :: forall a. WithCode (a -> m a),
      _ap     :: forall a b. WithCode (m (a -> b) -> m a -> m b)
    }

elimApplicative
  :: SynApplicative a
  -> ApplicativeDict m
  -> Code Q (m a)

elimApplicative (Return v) d@ApplicativeDict{..}
  = [|| $$(_code _return) $$(_code v) ||]

elimApplicative (App e1 e2) d@ApplicativeDict{..}
  = [|| $$(_code _ap) $$(elimApplicative e1 d) $$(elimApplicative e2 d) ||]

idAp :: Identity (a -> b) -> Identity a -> Identity b
idAp (Identity f) (Identity a) = Identity (f a)

identityDict = ApplicativeDict{..}
  where
    _return = WithCode Identity [|| Identity ||]
    _ap = WithCode idAp [|| idAp ||]
