{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

module Experiments.Ad.Examples where

import Control.Monad.Reader
import Criterion.Main
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Experiments.Ad.ExpGeneration
import Experiments.Ad.Abstract
import Experiments.Ad.Expressions
import Experiments.Ad.Forward
import Experiments.Ad.Reverse
import Generic.Random
import Language.Haskell.TH
import Language.Haskell.TH.Syntax.Compat

f1 :: Integer -> Integer
f1 y = forwardAD (\x -> y) exampleLarge ()

f1s :: Integer -> Integer
f1s y = $$(forwardADStaged (\x -> [||y :: Integer||]) exampleLargeStaged [||()||])

f2 :: Integer -> Integer
f2 y = abstractSharedGradientExtract (\x -> y) exampleLarge ()

f2s :: Integer -> Integer
f2s y = $$(abstractSharedGradientStagedExtract (\x -> [||y :: Integer||]) exampleLargeStaged [||()||])

f3 :: Integer -> Integer
f3 y = (fromMaybe 0 . M.lookup ()) (forwardADSparseExtract (\x -> y) exampleLarge)

f3s :: Integer -> Integer
f3s y = (fromMaybe 0 . M.lookup ()) $$(forwardADSparseStagedExtract (\x -> [||y :: Integer||]) exampleLargeStaged)

r1 :: Integer -> Integer
r1 y = (fromMaybe 0 . M.lookup ()) (reverseADExtract (\x -> y) exampleLarge)

r1s :: Integer -> Integer
r1s y = (fromMaybe 0 . M.lookup ()) $$(reverseADStagedExtract (\x -> [||y :: Integer||]) exampleLargeStaged)

r2 :: Integer -> Integer
r2 y = (fromMaybe 0 . M.lookup ()) (reverseADEndoExtract (\x -> y :: Integer) exampleLarge)

r2s :: Integer -> Integer
r2s y = (fromMaybe 0 . M.lookup ()) $$(reverseADEndoStagedExtract (\x -> [||y :: Integer||]) exampleLargeStaged)

r3IO :: Integer -> IO Integer
r3IO y = fmap (fromMaybe 0 . M.lookup ()) (reverseAD_CY_IO_extract (\x -> y :: Integer) exampleLarge ((), ()))

r3sIO :: Integer -> IO Integer
r3sIO y = fmap (fromMaybe 0 . M.lookup ()) $$(reverseAD_CY_IO_Staged_Extract (\x -> [||y :: Integer||]) exampleLargeStaged [||((), ())||])
