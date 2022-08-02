{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use tuple-section" #-}

module Eval2 where

import Language.Haskell.TH

data Expr v
  = Var v
  | Lit Int
  | Add (Expr v) (Expr v)
  | Mul (Expr v) (Expr v)
  deriving (Show)

class Eval a where
  var :: a
  lit :: Int -> a
  add :: a -> a -> a
  mul :: a -> a -> a

eval :: Eval a => (v -> a) -> Expr v -> a
eval var (Var v) = var v
eval _   (Lit n) = lit n
eval var (Add e1 e2) = eval var e1 `add` eval var e2
eval var (Mul e1 e2) = eval var e1 `mul` eval var e2

instance Eval (TExpQ Int -> TExpQ (Int, Int)) where
  var     = \v -> [|| ($$v, 1) ||]
  lit n   = \v -> [|| (n, 0) ||]
  add f g = \v -> [|| let (x, dx) = $$(f v)
                          (y, dy) = $$(f v)
                       in (x + y, dx + dy) ||]
  mul f g = \v -> [|| let (x, dx) = $$(f v)
                          (y, dy) = $$(g v)
                       in (x * y, dx * y + x * dy) ||]

instance Eval (Int -> (Int, Int)) where
  var     = \v -> (v, 1)
  lit n   = \v -> (n, 0)
  add f g = \v ->
    let (x, dx) = f v
        (y, dy) = g v
     in (x + y, dx + dy)
  mul f g = \v ->
    let (x, dx) = f v
        (y, dy) = g v
     in (x * y, dx * y + x * dy)



-- forwardAD :: (Eq v, Eval a) => (v -> a) -> v -> Expr v -> (a, a)
-- forwardAD var x = eval gen
--   where
--     gen y = (eval var y, eval pD y)
--     pD y = if x == y then Lit 1 else Lit 0
