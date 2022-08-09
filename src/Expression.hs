module Expression where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Semiring

data Expr v = Var v
            | Zero
            | One
            | Negative (Expr v)
            | Expr v :+: Expr v
            | Expr v :*: Expr v
            | Expr v :/: Expr v
            deriving (Show, Generic)

infixl 5 :*:
infixl 4 :+:

instance NFData v => NFData (Expr v)

instance Semiring (Expr v) where
  zero = Zero
  one = One
  Zero `plus` rhs = rhs
  lhs `plus` Zero = lhs
  lhs `plus` Negative Zero = lhs
  Negative Zero `plus` rhs = rhs
  lhs `plus` rhs = lhs :+: rhs
  One `times` rhs = rhs
  lhs `times` One = lhs
  Negative One `times` rhs = Negative rhs
  lhs `times` Negative One = Negative lhs
  Zero `times` _ = Zero
  _ `times` Zero = Zero
  lhs `times` rhs = lhs :*: rhs

instance Num (Expr v) where
  (+) = plus
  (*) = times
  negate (Negative e) = e
  negate e = Negative e
  -- TODO
  fromInteger 0 = Zero
  fromInteger 1 = One
  fromInteger n = One :+: fromInteger (n - 1)
  abs = undefined
  signum = undefined

instance Fractional (Expr v) where
  lhs / One = lhs
  lhs / Negative One = Negative lhs
  Zero / rhs = Zero
  Negative Zero / rhs = Zero
  lhs / rhs = lhs :/: rhs
  fromRational = undefined -- TODO

eval :: (Fractional d, Semiring d) => (v -> d) -> Expr v -> d
eval lookup (Var v) = lookup v
eval _ Zero = zero
eval _ One = one
eval lookup (Negative e) = negate $ eval lookup e
eval lookup (lhs :+: rhs) = eval lookup lhs + eval lookup rhs
eval lookup (lhs :*: rhs) = eval lookup lhs * eval lookup rhs
eval lookup (lhs :/: rhs) = eval lookup lhs / eval lookup rhs
