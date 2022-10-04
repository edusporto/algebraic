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
import Experiments.Ad.Abstract
import Experiments.Ad.Examples
import Experiments.Ad.ExpGeneration
import Experiments.Ad.Expressions
import Experiments.Ad.Forward
import Experiments.Ad.Reverse
import Generic.Random
import Language.Haskell.TH
import Language.Haskell.TH.Syntax.Compat

main :: IO ()
main =
  defaultMain
    [ bgroup
        "AD"
        [ bench "forwardAD" $ nf f1 2,
          bench "forwardADStaged" $ nf f1s 2,
          bench "abstractSharedGradient" $ nf f2 2,
          bench "abstractSharedGradientStaged" $ nf f2s 2,
          bench "forwardADSparse" $ nf f3 2,
          bench "forwardADSparseStaged" $ nf f3s 2,
          bench "reverseAD" $ nf r1 2,
          bench "reverseADStaged" $ nf r1s 2,
          bench "reverseADEndo" $ nf r2 2,
          bench "reverseADEndoStaged" $ nf r2s 2,
          bench "reverseAD_CY_IO" $ nfIO (r3IO 2),
          bench "reverseAD_CY_IO_Staged" $ nfIO (r3sIO 2)
        ]
    ]

{-
benchmarking AD/forwardAD
time                 58.24 μs   (53.33 μs .. 66.04 μs)
                     0.921 R²   (0.863 R² .. 0.991 R²)
mean                 54.79 μs   (51.68 μs .. 60.85 μs)
std dev              13.33 μs   (7.097 μs .. 20.03 μs)
variance introduced by outliers: 97% (severely inflated)

benchmarking AD/forwardADStaged
time                 8.333 μs   (8.279 μs .. 8.415 μs)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 8.607 μs   (8.482 μs .. 8.859 μs)
std dev              628.5 ns   (300.4 ns .. 1.202 μs)
variance introduced by outliers: 77% (severely inflated)

benchmarking AD/abstractSharedGradient
time                 46.52 μs   (45.40 μs .. 47.99 μs)
                     0.984 R²   (0.954 R² .. 0.998 R²)
mean                 46.32 μs   (45.12 μs .. 49.94 μs)
std dev              6.624 μs   (2.052 μs .. 13.56 μs)
variance introduced by outliers: 91% (severely inflated)

benchmarking AD/abstractSharedGradientStaged
time                 8.303 μs   (8.256 μs .. 8.353 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.345 μs   (8.301 μs .. 8.408 μs)
std dev              173.7 ns   (138.1 ns .. 219.4 ns)
variance introduced by outliers: 21% (moderately inflated)

benchmarking AD/forwardADSparse
time                 74.52 μs   (72.17 μs .. 77.15 μs)
                     0.988 R²   (0.978 R² .. 0.998 R²)
mean                 73.48 μs   (71.85 μs .. 76.24 μs)
std dev              7.082 μs   (4.582 μs .. 11.70 μs)
variance introduced by outliers: 81% (severely inflated)

benchmarking AD/forwardADSparseStaged
time                 38.27 μs   (37.19 μs .. 39.17 μs)
                     0.995 R²   (0.993 R² .. 0.997 R²)
mean                 38.57 μs   (37.96 μs .. 39.27 μs)
std dev              2.138 μs   (1.688 μs .. 3.247 μs)
variance introduced by outliers: 61% (severely inflated)

benchmarking AD/reverseAD
time                 68.82 μs   (67.09 μs .. 70.92 μs)
                     0.995 R²   (0.992 R² .. 0.998 R²)
mean                 67.82 μs   (66.84 μs .. 69.18 μs)
std dev              4.105 μs   (3.210 μs .. 5.172 μs)
variance introduced by outliers: 63% (severely inflated)

benchmarking AD/reverseADStaged
time                 32.24 μs   (31.67 μs .. 32.66 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 32.72 μs   (32.28 μs .. 33.23 μs)
std dev              1.609 μs   (1.348 μs .. 1.915 μs)
variance introduced by outliers: 56% (severely inflated)

benchmarking AD/reverseADEndo
time                 140.6 μs   (137.4 μs .. 144.9 μs)
                     0.992 R²   (0.982 R² .. 0.998 R²)
mean                 142.6 μs   (140.4 μs .. 147.5 μs)
std dev              10.84 μs   (6.892 μs .. 18.58 μs)
variance introduced by outliers: 71% (severely inflated)

benchmarking AD/reverseADEndoStaged
time                 8.106 μs   (7.968 μs .. 8.248 μs)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 8.389 μs   (8.283 μs .. 8.628 μs)
std dev              526.3 ns   (353.3 ns .. 960.8 ns)
variance introduced by outliers: 71% (severely inflated)

benchmarking AD/reverseAD_CY_IO
time                 500.4 μs   (482.5 μs .. 530.2 μs)
                     0.955 R²   (0.896 R² .. 0.996 R²)
mean                 487.3 μs   (468.6 μs .. 543.8 μs)
std dev              94.01 μs   (54.09 μs .. 159.5 μs)
variance introduced by outliers: 93% (severely inflated)

benchmarking AD/reverseAD_CY_IO_Staged
time                 173.8 μs   (153.3 μs .. 205.9 μs)
                     0.871 R²   (0.779 R² .. 0.993 R²)
mean                 155.4 μs   (147.4 μs .. 172.2 μs)
std dev              38.75 μs   (13.27 μs .. 78.05 μs)
variance introduced by outliers: 97% (severely inflated)

-------------------------

-- With profiling on:

benchmarking AD/forwardAD
time                 140.5 μs   (135.0 μs .. 147.3 μs)
                     0.969 R²   (0.955 R² .. 0.983 R²)
mean                 129.8 μs   (121.2 μs .. 139.5 μs)
std dev              27.86 μs   (23.81 μs .. 34.55 μs)
variance introduced by outliers: 95% (severely inflated)

benchmarking AD/forwardADStaged
time                 74.16 μs   (69.84 μs .. 80.87 μs)
                     0.968 R²   (0.953 R² .. 0.989 R²)
mean                 88.05 μs   (83.36 μs .. 95.91 μs)
std dev              20.71 μs   (14.61 μs .. 31.08 μs)
variance introduced by outliers: 96% (severely inflated)

benchmarking AD/abstractSharedGradient
time                 161.2 μs   (154.7 μs .. 174.9 μs)
                     0.889 R²   (0.794 R² .. 0.963 R²)
mean                 245.7 μs   (215.9 μs .. 299.2 μs)
std dev              137.3 μs   (79.73 μs .. 233.7 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking AD/abstractSharedGradientStaged
time                 230.4 μs   (224.8 μs .. 237.8 μs)
                     0.990 R²   (0.986 R² .. 0.993 R²)
mean                 280.4 μs   (266.8 μs .. 294.7 μs)
std dev              47.02 μs   (39.04 μs .. 57.00 μs)
variance introduced by outliers: 91% (severely inflated)

benchmarking AD/forwardADSparse
time                 232.8 μs   (226.4 μs .. 241.1 μs)
                     0.968 R²   (0.936 R² .. 0.983 R²)
mean                 314.1 μs   (285.7 μs .. 384.2 μs)
std dev              147.6 μs   (69.73 μs .. 278.3 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking AD/forwardADSparseStaged
time                 101.8 μs   (95.16 μs .. 107.8 μs)
                     0.974 R²   (0.963 R² .. 0.985 R²)
mean                 112.4 μs   (104.5 μs .. 139.8 μs)
std dev              44.24 μs   (15.36 μs .. 95.77 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking AD/reverseAD
time                 326.2 μs   (308.6 μs .. 343.3 μs)
                     0.978 R²   (0.966 R² .. 0.991 R²)
mean                 392.3 μs   (354.7 μs .. 524.3 μs)
std dev              230.9 μs   (61.98 μs .. 479.0 μs)
variance introduced by outliers: 99% (severely inflated)

benchmarking AD/reverseADStaged
time                 223.7 μs   (213.1 μs .. 234.0 μs)
                     0.982 R²   (0.974 R² .. 0.991 R²)
mean                 249.5 μs   (240.2 μs .. 270.1 μs)
std dev              48.53 μs   (24.11 μs .. 98.74 μs)
variance introduced by outliers: 94% (severely inflated)

benchmarking AD/reverseADEndo
time                 342.3 μs   (329.6 μs .. 361.6 μs)
                     0.970 R²   (0.952 R² .. 0.985 R²)
mean                 361.3 μs   (347.9 μs .. 379.8 μs)
std dev              54.33 μs   (42.18 μs .. 71.21 μs)
variance introduced by outliers: 89% (severely inflated)

benchmarking AD/reverseADEndoStaged
time                 451.7 μs   (446.1 μs .. 460.2 μs)
                     0.994 R²   (0.985 R² .. 0.998 R²)
mean                 528.4 μs   (511.8 μs .. 557.1 μs)
std dev              68.79 μs   (46.53 μs .. 115.1 μs)
variance introduced by outliers: 85% (severely inflated)

benchmarking AD/reverseAD_CY_IO
time                 1.037 ms   (995.8 μs .. 1.082 ms)
                     0.983 R²   (0.973 R² .. 0.989 R²)
mean                 1.220 ms   (1.163 ms .. 1.325 ms)
std dev              279.0 μs   (209.9 μs .. 409.8 μs)
variance introduced by outliers: 94% (severely inflated)

benchmarking AD/reverseAD_CY_IO_Staged
time                 1.258 ms   (965.4 μs .. 1.548 ms)
                     0.838 R²   (0.784 R² .. 0.984 R²)
mean                 1.095 ms   (1.044 ms .. 1.188 ms)
std dev              216.1 μs   (118.2 μs .. 380.8 μs)
variance introduced by outliers: 92% (severely inflated)
-}
