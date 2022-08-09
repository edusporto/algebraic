module Lib where

import Prelude hiding (map)

import Criterion.Main

import GHC.Generics (Generic)
import Control.Exception (evaluate)
import Control.DeepSeq (force, NFData(..))
import Control.Monad (forM_)
import           Data.Map (Map)
import qualified Data.Map as Map
import Data.List (foldl1')

import Expression
import Semiring
import PrettyPrinting
import Reverse
import Forward
import FnSpace
import Abstract
import Utils

import Line
import qualified Hyperplane as Hyper

{-
-- data X = X deriving (Eq, Ord, Show)
data XY = X | Y deriving (Eq, Ord, Show)

--  f = x * (x + 1) = x^2 + x
-- df = 2x + 1
example1 = Var X :*: (Var X :+: one)

-- (x + 1) * 1 + x * (1 + 0)
forwardRes = forwardAD Var example2 X

-- ((x + 1) * 1) * 1 + ((x * 1) * 1 + 0)
-- reverseRes = reverseAD Var example1 X

example2 = Var X :*: (Var Y :+: one) :*: Var X :*: Var X :*: Var X :+: Var Y :+: Var X :+: Var X

forwardRes2 = forwardAD Var example2 X

-- reverseRes2 = reverseEndoAD Var example2

reverseEndoRes = reverseEndoAD Var example2 X

data Net d = { w11 :: d
             , w12 :: d
             , w13 :: d
             , w21 :: d
             , w22 :: d
             , w23 :: d
             , w31 :: d
             , w32 :: d
             , w33 :: d
             , w41 :: d
             , w42 :: d
             , w43 :: d
             , b1 :: d
             , b2 :: d
             , b3 :: d
             }

data NetVar = I1 | I2 | I3 | I4 -- Inputs
            | O1 | O2 | O3 -- Outputs
            | W11 | W12 | W13 | W21 | W22 | W23 | W31 | W32 | W33 -- Weights
            | B1 | B2 | B3 -- Biases

netExpr :: Expr NetVar
netExpr = let out1 = Var I1 :*: Var W11
                 :+: Var I2 :*: Var W21
                 :+: Var I3 :*: Var W31
                 :+: Var I4 :*: Var W41
                 :+: Var B1

              out2 = Var I1 :*: Var W12
                 :+: Var I2 :*: Var W22
                 :+: Var I3 :*: Var W32
                 :+: Var I4 :*: Var W42
                 :+: Var B2

              out3 = Var I1 :*: Var W13
                 :+: Var I2 :*: Var W23
                 :+: Var I3 :*: Var W33
                 :+: Var I4 :*: Var W43
                 :+: Var B3

           in (out1 :-: Var O1) :*: (out1 :-: Var O1)
          :+: (out2 :-: Var O2) :*: (out2 :-: Var O2)
          :+: (out3 :-: Var O2) :*: (out2 :-: Var O2)
-}

run = defaultMain [
      env setupEnv $ \ ~(dat, line) -> bgroup "simple linear regression" [
          bench "forward" $ nf (updateLineGrad forwardMethod dat) line
        , bench "reverse" $ nf (updateLineGrad reverseMethod dat) line
        , bench "reverse direct" $ nf (updateLineGradDirectReverse dat) line
      ]
    , env Hyper.setupEnv $ \ ~(dim, dat, p) -> bgroup "hyperplane" [
        bench "forward" $ nf (Hyper.updateGrad (Hyper.forwardMethod dim) dim dat) p
      , bench "reverse" $ nf (Hyper.updateGrad (Hyper.reverseMethod dim) dim dat) p
      ]
  ]
