{-# LANGUAGE TemplateHaskell #-}

module Experiments.TH.Example where

import Language.Haskell.TH

add1 :: Int -> Q Exp
add1 x = [|x + 1|]

oneC, twoC, plusC :: Q Exp
oneC = [|1|]
twoC = [|2|]
plusC = [|$oneC + $twoC|]





