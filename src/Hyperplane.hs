module Hyperplane where

import           GHC.Arr ((!), amap)
import qualified GHC.Arr as Array
import           GHC.Generics (Generic)
import           Data.Word (Word64)
import           Control.DeepSeq (NFData)
import qualified Data.Map as Map

import           Expression (Expr)
import qualified Expression as Expr
import           Utils
import           Semiring
import           Forward
import           Reverse

type Dim = Word64

type Array = Array.Array Dim
array = Array.array . (,) 1
size = snd . Array.bounds
listToArray list = array len $ zip [1..len] list
  where len = fromIntegral $ length list

-- Xi * Wi + D
data Hyperplane d = Hyperplane { hyperW :: Array d
                               , hyperD :: d
                               }
                               deriving (Generic)

instance NFData d => NFData (Hyperplane d)

data Var = X Dim | W Dim | D | Out deriving (Show, Eq, Ord, Generic)

fExpr :: Dim -> Expr Var
fExpr dim = sum [Expr.Var (X i) * Expr.Var (W i) | i <- [1..dim]] + Expr.Var D

sqErrorExpr :: Dim -> Expr Var
sqErrorExpr dim = (Expr.Var Out - fExpr dim)^2

evalExpr :: Expr Var -> Hyperplane Double -> (Array Double, Double) -> Double
evalExpr expr line (x, y) = Expr.eval lookup expr
  where lookup (X i) = x ! i
        lookup Out = y
        lookup (W i) = hyperW line ! i
        lookup D = hyperD line

evalMeanExpr :: Hyperplane Double -> Expr Var -> [(Array Double, Double)] -> Double
evalMeanExpr line expr dat = mean $ fmap (evalExpr expr line) dat

genArray :: Dim -> (Dim -> a) -> Array a
genArray dim f = array dim $ zip [1..dim] $ map f [1..dim]

azipWith :: (a -> b -> c) -> Array a -> Array b -> Array c
azipWith f a b = array (size a) [(i, f (a ! i) (b ! i)) | i <- (Array.indices a)]

updateGrad :: (Var -> Expr Var)
           -> Dim
           -> [(Array Double, Double)]
           -> Hyperplane Double
           -> Hyperplane Double
updateGrad method dim dat p = let wGrad = genArray dim $ \i -> evalMeanExpr p (method (W i)) dat
                                  dGrad = evalMeanExpr p (method D) dat
                               in Hyperplane { hyperW = azipWith (-) (hyperW p) (amap (lr*) wGrad)
                                             , hyperD = hyperD p - lr * dGrad
                                             }
                               where lr = 1e-2

forwardMethod :: Dim -> Var -> Expr Var
forwardMethod dim = forwardAD Expr.Var (sqErrorExpr dim)

reverseMethod :: Dim -> Var -> Expr Var
reverseMethod dim = maybe zero id . flip Map.lookup reverseMap
  where reverseMap = reverseEndoMapAD Expr.Var (sqErrorExpr dim)

setupEnv :: IO (Dim, [(Array Double, Double)], Hyperplane Double)
setupEnv = do
  let dim = 10
  let p = Hyperplane { hyperW = genArray dim $ const 1
                     , hyperD = 0
                     }
  return (dim, sampleData, p)

sampleData :: [(Array Double, Double)]
sampleData = [ (listToArray [1, 2, 1, 1, 1, 1, 1, 1, 1, 1],  3)
             , (listToArray [3, 2, 1, 1, 1, 1, 1, 1, 1, 1],  5)
             , (listToArray [2, 2, 1, 1, 1, 1, 1, 1, 1, 1],  4)
             , (listToArray [2, 5, 1, 1, 1, 1, 1, 1, 1, 1],  7)
             , (listToArray [3, 7, 1, 1, 1, 1, 1, 1, 1, 1], 10)
             , (listToArray [1, 0, 1, 1, 1, 1, 1, 1, 1, 1],  1)
             , (listToArray [0, 1, 1, 1, 1, 1, 1, 1, 1, 1],  1)
             ]
