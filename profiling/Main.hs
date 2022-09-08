{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

module Main where

import Control.Monad.Reader
import Criterion.Main
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Experiments.Ad.Examples
import Experiments.Ad.ExpGeneration
import Experiments.Ad.Abstract
import Experiments.Ad.Expressions
import Experiments.Ad.Forward
import Experiments.Ad.Reverse
import Generic.Random
import Language.Haskell.TH
import Language.Haskell.TH.Syntax.Compat

main :: IO ()
main = do
  -- exp <- randomExpression
  print $ "F1:  " <> show (f1 2)
  print $ "F1S: " <> show (f1s 2)
  print $ "F2:  " <> show (f2 2)
  print $ "F2S: " <> show (f2s 2)
  print $ "F3:  " <> show (f1 2)
  print $ "F3S: " <> show (f1s 2)
  print $ "R1:  " <> show (r1 2)
  print $ "R1S: " <> show (r1s 2)
  print $ "R2:  " <> show (r2 2)
  print $ "R2S: " <> show (r2s 2)
  r3 <- r3IO 2
  print $ "R3:  " <> show r3
  r3s <- r3sIO 2
  print $ "R3s: " <> show r3s
