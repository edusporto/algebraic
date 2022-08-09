module Line where

import GHC.Generics (Generic)
import           Data.Map (Map)
import qualified Data.Map as Map
import Control.DeepSeq (NFData, force)
import Data.List (foldl1')

import Semiring
import Expression
import Reverse
import Forward
import Utils

data Line d = Line { angCoef :: d, linCoef :: d } deriving Generic

instance NFData d => NFData (Line d)

data LineVar = X | AngCoef | LinCoef | Out deriving (Show, Eq, Ord, Generic)

instance NFData LineVar

lineExpr :: Expr LineVar
lineExpr = Var X :*: Var AngCoef :+: Var LinCoef

lineSquaredError :: Expr LineVar
lineSquaredError = (Var Out - lineExpr)^2

predict :: Line Double -> Double -> Double
predict line x = eval lookup lineExpr
  where lookup X = x
        lookup AngCoef = angCoef line
        lookup LinCoef = linCoef line

        -- I don't like this...
        lookup Out = undefined

evalLineExpr :: Expr LineVar -> Line Double -> (Double, Double) -> Double
evalLineExpr expr line (x, y) = eval lookup expr
  where lookup X = x
        lookup Out = y
        lookup AngCoef = angCoef line
        lookup LinCoef = linCoef line

evalLineSquaredError :: Line Double -> (Double, Double) -> Double
evalLineSquaredError = evalLineExpr lineSquaredError

evalLineMeanSquaredError :: Line Double -> [(Double, Double)] -> Double
evalLineMeanSquaredError line = mean . fmap (evalLineSquaredError line)

evalMeanGrad :: Line Double -> Expr LineVar -> [(Double, Double)] -> Double
evalMeanGrad line expr dat = mean $ fmap (evalLineExpr expr line) dat

updateLineGrad :: (LineVar -> Expr LineVar) -> [(Double, Double)] -> Line Double -> Line Double
updateLineGrad method dat line = let angCoefGradVal = evalMeanGrad line (method AngCoef) dat
                                     linCoefGradVal = evalMeanGrad line (method LinCoef) dat
                                  in Line { angCoef = angCoef line - lr * angCoefGradVal
                                          , linCoef = linCoef line - lr * linCoefGradVal
                                          }
                                  where lr = 1e-2

forwardMethod :: LineVar -> Expr LineVar
forwardMethod = forwardAD Var lineSquaredError

-- Can't just use `reverseEndoAD` directly since that would recalculate the Map every time.
reverseMethod :: LineVar -> Expr LineVar
reverseMethod = maybe zero id . flip Map.lookup reverseMap
  where reverseMap = reverseEndoMapAD Var lineSquaredError

lineDirectUpdateMap :: Line Double -> (Double, Double) -> Map LineVar Double
lineDirectUpdateMap line (x, y) = reverseEndoMapAD lookup lineSquaredError
  where lookup X = x
        lookup Out = y
        lookup AngCoef = angCoef line
        lookup LinCoef = linCoef line

lineDirectUpdateMapMean :: Line Double -> [(Double, Double)] -> Map LineVar Double
lineDirectUpdateMapMean line dat = fmap (/n) $ foldl1' (\l r -> force $ Map.unionWith (+) l r) $ gradients
  where gradients = fmap (lineDirectUpdateMap line) dat
        n = fromIntegral $ length dat

updateLineGradDirectReverse :: [(Double, Double)] -> Line Double -> Line Double
updateLineGradDirectReverse dat line = Line { angCoef = angCoef line - lr * gradOf AngCoef
                                            , linCoef = linCoef line - lr * gradOf LinCoef
                                            }
  where lr = 1e-2
        gradOf = maybe zero id . flip Map.lookup map
        map = lineDirectUpdateMapMean line dat

setupEnv :: IO ([(Double, Double)], Line Double)
setupEnv = do
  let dat = [(1, 2), (2, 4), (4, 8), (6, 12), (8, 16)]
  let line = Line { angCoef = 1, linCoef = 0 } :: Line Double
  return (dat, line)
