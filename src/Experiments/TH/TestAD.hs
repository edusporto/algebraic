{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Use tuple-section" #-}

module Experiments.TH.TestAD where

import Experiments.TH.AD
import Language.Haskell.TH

fun :: Int -> (Int, Int)
fun = $$([||\y -> $$(forwardADStaged (\x -> [||y||]) [||X||] example2 :: Code Q (Int, Int))||])

-- NO custom Semiring (Code Q (Dual d))
-- fun :: Int -> (Int, Int)
-- fun = \y
--   -> ((y, if (X == X) then one else zero)
--         `mul` ((y, if (X == X) then one else zero) `add` one))

-- Custom Semiring (Code Q (Dual d))
-- fun :: Int -> (Int, Int)
-- fun y =
--   let (f, df) = (y, if X == X then one else zero)
--       (g, dg) =
--         let (f, df) = (y, if X == X then one else zero)
--             (g, dg) = (one, zero)
--          in (f `add` g, df `add` dg)
--    in (f `mul` g, (g `mul` df) `add` (f `mul` dg))

-- Good:
{-
\ y_a3CJ
      -> let
           (f_a3CM, df_a3CN) = (y_a3CJ, if (X == X) then one else zero)
           (g_a3CK, dg_a3CL)
             = let
                 (f_a3CQ, df_a3CR) = (y_a3CJ, if (X == X) then one else zero)
                 (g_a3CO, dg_a3CP) = (one, zero)
               in ((f_a3CQ `add` g_a3CO), (df_a3CR `add` dg_a3CP))
         in
           ((f_a3CM `mul` g_a3CK),
            ((g_a3CK `mul` df_a3CN) `add` (f_a3CM `mul` dg_a3CL)))
-}

-- Bad:
{-
 \ y_a4hO
      -> let
           (f_a4hR, df_a4hS)
             = (y_a4hO,
                ((\ x_a4hT y_a4hU -> if (x_a4hT == y_a4hU) then one else zero) X)
                  X)
           (g_a4hP, dg_a4hQ)
             = let
                 (f_a4hX, df_a4hY)
                   = (y_a4hO,
                      ((\ x_a4hZ y_a4i0 -> if (x_a4hZ == y_a4i0) then one else zero) X)
                        X)
                 (g_a4hV, dg_a4hW) = (one, zero)
               in ((f_a4hX `add` g_a4hV), (df_a4hY `add` dg_a4hW))
         in
           ((f_a4hR `mul` g_a4hP),
            ((g_a4hP `mul` df_a4hS) `add` (f_a4hR `mul` dg_a4hQ)))
-}
