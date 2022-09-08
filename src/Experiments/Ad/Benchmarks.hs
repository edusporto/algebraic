{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

module Experiments.Ad.Benchmarks where

import Control.Monad.Reader
import Data.Array.IO
import Data.Map
import Experiments.Ad.Abstract
import Experiments.Ad.Expressions
import Experiments.Ad.Forward
import Experiments.Ad.Reverse
import Language.Haskell.TH
import Language.Haskell.TH.Syntax.Compat

import Test.QuickCheck
import Generic.Random

vars :: SpliceQ X
vars = [||X||]

instance Arbitrary a => Arbitrary (Expr a) where
  arbitrary = genericArbitraryRec uniform `withBaseCase` return One

main :: IO ()
main = sample (arbitrary :: Gen (Expr ()))
