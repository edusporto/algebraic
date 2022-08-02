{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use tuple-section" #-}

module Eval where

import Language.Haskell.TH

data Expr = Lit Int | Add Expr Expr | Var | Mul Expr Expr
  deriving (Show)

class Eval a where
  var :: a
  lit :: Int -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Eval Int where
  var = 0
  lit = id
  add = (+)
  mul = (*)

instance Eval (Int -> Int) where
  var     = \v -> v
  lit n   = \v -> n
  add f g = \v -> f v + g v
  mul f g = \v -> f v * g v

instance Eval (TExpQ (Int -> Int)) where
  var     = [||\v -> v||]
  lit n   = [||\v -> n||]
  add f g = [||\v -> $$f v + $$g v||]
  mul f g = [||\v -> $$f v * $$g v||]

instance Eval (Int -> TExpQ Int) where
  var     = \v -> [||v||]
  lit n   = \v -> [||n||]
  add f g = \v -> [||$$(f v) + $$(g v)||]
  mul f g = \v -> [||$$(f v) * $$(g v)||]

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

instance Eval (Int -> TExpQ (Int, Int)) where
  var     = \v -> [|| (v, 1) ||]
  lit n   = \v -> [|| (n, 0) ||]
  add f g = \v -> [|| let (x, dx) = $$(f v)
                          (y, dy) = $$(g v)
                       in (x + y, dx + dy) ||]
  mul f g = \v -> [|| let (x, dx) = $$(f v)
                          (y, dy) = $$(g v)
                       in (x * y, dx * y + x * dy) ||]

eval :: Eval a => Expr -> a
eval Var = var
eval (Lit n) = lit n
eval (Add e1 e2) = eval e1 `add` eval e2
eval (Mul e1 e2) = eval e1 `mul` eval e2

-- evalStaged (Add Var (Add (Lit 3) (Lit 5)) ~> \v -> v + 8

-- >>> f = eval (Add Var (Add (Lit 3) (Lit 5))) :: Int -> TExpQ (Int, Int)
-- >>> $$(f 1)
-- >>> $$(f 2)
-- >>> $$(f 20)
-- (9,1)
-- (10,1)
-- (28,1)

instance Eval (TExpQ Int -> TExpQ (Int, Int)) where
  var     = \v -> [|| ($$v, 1) ||]
  lit n   = \v -> [|| (n, 0) ||]
  add f g = \v -> [|| let (x, dx) = $$(f v)
                          (y, dy) = $$(f v)
                       in (x + y, dx + dy) ||]
  mul f g = \v -> [|| let (x, dx) = $$(f v)
                          (y, dy) = $$(g v)
                       in (x * y, dx * y + x * dy) ||]
