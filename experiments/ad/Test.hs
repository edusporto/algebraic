{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}

module Test where

import Language.Haskell.TH
import Data.Map

import Expressions
import Abstract
import Forward
import Reverse

-- >>> reverseADExtract (\X -> 5) example3
-- fromList [(X,170)]

fun :: Int -> Map X Int
fun = $$([||\y -> $$(reverseADStagedExtract (\x -> [||y||]) example1Staged) ||])

-- fun = \ y
--   -> (sparseSA
--         $ (absHom
--              $ eCW
--                  ((CW y)
--                     (Hom (\d -> Sparse ((singleton X) (SA d))))
--                     `times`
--                       ((CW y)
--                          (Hom (\d -> Sparse ((singleton X) (SA d)))) `plus` one))))


