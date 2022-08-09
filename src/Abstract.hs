module Abstract where

import GHC.Base (liftA2)

import Semiring
import Expression

-- The idea is that `d` is the type of the operation. This would be something like `Int`, or
-- `FLoat`. The `e` type, however is the derivative type of `d`. `e` might be just a `Float`, but
-- it could also be a vector representing every partial derivative of `f` with respect to each
-- variable.
data Dual d e = Dual { f :: d, df :: e }

-- instance Functor (Dual d) where
--     fmap g (Dual f df) = Dual f (g df)

instance Module d e => Semiring (Dual d e) where
    zero = Dual one mempty
    one  = Dual one mempty
    (Dual f df) `plus`  (Dual g dg) = Dual (f `plus` g) (df <> dg)
    (Dual f df) `times` (Dual g dg) = Dual (f `times` g) ((g `scalarMult` df) <> (f `scalarMult` dg))

instance (Module d e, Num d, Num e) => Num (Dual d e) where
  (+) = plus
  (*) = times
  negate (Dual f df) = Dual (negate f) (negate df)
  fromInteger = flip Dual mempty . fromInteger
  -- Is this correct???
  abs (Dual f df) = Dual (abs f) (abs df)
  signum (Dual f df) = Dual (signum f) (signum df)

instance (Module d e, Fractional d, Num e) => Fractional (Dual d e) where
  -- Is this correct??? The idea is that we want (f'/g')' = (f'g - g'f) / g^2, but we need to
  -- change a couple of things in order to make things work.
  (Dual f df) / (Dual g dg) = Dual (f / g) $
    (recip (g ^ 2)) `scalarMult` ((g `scalarMult` df) - (f `scalarMult` dg))
  fromRational = undefined -- TODO

abstractD :: (Fractional d, Num e, Kronecker v d e) => (v -> d) -> Expr v -> Dual d e
abstractD gen = eval env
    where env x = Dual (gen x) (delta x)
