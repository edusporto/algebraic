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

-- f1' = f1 2
-- f1's = f1s 2
-- f2' = f2 2
-- f2's = f2s 2
-- f3' = f3 2
-- f3's = f3s 2
-- r1' = r1 2
-- r1's = r1s 2
-- r2' = r2 2
-- r2's = r2s 2

testFunc y = $$(forwardADStaged (\x -> [||y :: Integer||]) exampleSmall1Staged [||X||])
-- testFunc y = forwardAD (\x -> y :: Integer) exampleSmall1 X
-- testFunc y = $$(forwardADStaged (\x -> [||y :: Integer||]) exampleLargeStaged [||()||])
-- testFunc y = (fromMaybe 0 . M.lookup X) $$(reverseADEndoStagedExtract (\x -> [||y :: Integer||]) exampleSmall1Staged)
-- testFunc y = (fromMaybe 0 . M.lookup ()) $$(reverseADEndoStagedExtract (\x -> [||y :: Integer||]) exampleLargeStaged)
testVal = testFunc 2

main :: IO ()
main =
  print testVal

-- main :: IO ()
-- main = do
--   -- exp <- randomExpression
--   print $ "F1:  " <> show (f1 2)
--   print $ "F1S: " <> show (f1s 2)
--   print $ "F2:  " <> show (f2 2)
--   print $ "F2S: " <> show (f2s 2)
--   print $ "F3:  " <> show (f3 2)
--   print $ "F3S: " <> show (f3s 2)
--   print $ "R1:  " <> show (r1 2)
--   print $ "R1S: " <> show (r1s 2)
--   print $ "R2:  " <> show (r2 2)
--   print $ "R2S: " <> show (r2s 2)
--   r3 <- r3IO 2
--   print $ "R3:  " <> show r3
--   r3s <- r3sIO 2
--   print $ "R3s: " <> show r3s


-- Notes:
-- [|| if $$x == $$y then one else zero ||]
-- Use a custom expression for equality
