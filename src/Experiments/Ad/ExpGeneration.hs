{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

module Experiments.Ad.ExpGeneration where

import Control.Monad.Reader
import Data.Array.IO
import qualified Data.Map as M
import Experiments.Ad.Abstract
import Experiments.Ad.Expressions
import Experiments.Ad.Forward
import Experiments.Ad.Reverse
import Language.Haskell.TH
import Language.Haskell.TH.Syntax.Compat

import Test.QuickCheck
import Generic.Random
import Data.Maybe (fromJust)

vars :: SpliceQ X
vars = [||X||]

-- instance Arbitrary a => Arbitrary (Expr a) where
--   arbitrary = genericArbitraryRec (1 % 1 % 1 % 1 % 1 % ())

instance Arbitrary a => Arbitrary (Expr a) where
  arbitrary = sized $ \n ->
    if n == 0 then
      Var <$> arbitrary
    else
      frequency
        [ (1, resize (max 0 (n - 1)) (pure Zero))
        , (2, resize (max 0 (n - 1)) (pure One))
        , (2, resize (max 0 (n - 1)) (Var <$> arbitrary))
        , (4, resize (n `div` 2)     (Plus <$> arbitrary <*> arbitrary))
        , (4, resize (n `div` 2)     (Times <$> arbitrary <*> arbitrary))
        ]

-- arbitrary :: Arbitrary a => Gen (Tree a)
-- arbitrary = sized $ \n ->
--   -- "if" condition from withBaseCase
--   if n == 0 then
--     Leaf <$> arbitrary
--   else
--     -- genericArbitraryRec
--     frequency
--       [ (1, resize (max 0 (n - 1)) (Leaf <$> arbitrary))
--       , (2, resize (n `div` 2)     (Node <$> arbitrary <*> arbitrary))
--       ]

randomExpression :: IO (Expr ())
randomExpression = generate $ resize 10000 (arbitrary :: Gen (Expr ()))

exampleSmall1 :: Expr X
exampleSmall1 = Times (Var X) (Plus (Var X) One)
exampleSmall1Staged :: Expr (SpliceQ X)
exampleSmall1Staged = Times (Var [||X||]) (Plus (Var [||X||]) One)

exampleLarge :: Expr ()
exampleLarge = Times (Plus (Times (Times (Times (Plus (Plus (Plus (Plus (Plus (Times (Plus One (Plus (Plus (Var ()) (Var ())) (Times (Var ()) (Var ())))) (Times One (Var ()))) Zero) (Var ())) Zero) (Times (Times (Plus One (Var ())) (Plus (Times (Times (Plus One (Var ())) (Var ())) (Var ())) (Plus (Plus (Var ()) (Plus (Times (Var ()) (Var ())) (Times (Var ()) (Var ())))) (Plus (Plus (Plus (Var ()) (Var ())) (Times (Var ()) (Var ()))) (Times Zero One))))) One)) (Times (Plus (Var ()) (Var ())) (Times (Times (Var ()) (Times (Times (Times (Times (Plus (Var ()) (Var ())) (Var ())) (Times (Times (Var ()) (Var ())) (Plus (Var ()) (Var ())))) (Var ())) (Times (Times One (Var ())) One))) (Var ())))) (Times (Plus (Times (Times Zero (Times (Times (Times One One) (Times One (Times (Times (Var ()) (Var ())) (Times (Var ()) (Var ()))))) (Times One Zero))) One) Zero) (Plus (Plus (Plus Zero One) (Var ())) (Times Zero (Var ()))))) (Plus (Plus Zero (Var ())) (Plus (Times (Times (Plus One (Plus (Plus (Plus (Plus Zero (Var ())) (Times (Plus (Var ()) (Var ())) (Var ()))) (Times (Plus (Var ()) (Times (Var ()) (Var ()))) (Times (Plus (Var ()) (Var ())) (Times (Var ()) (Var ()))))) (Times (Times One (Times (Times (Var ()) (Var ())) (Var ()))) One))) (Times (Var ()) (Plus (Plus (Times One (Times One (Plus (Var ()) (Var ())))) (Plus (Plus (Var ()) (Plus (Var ()) (Var ()))) Zero)) One))) One) (Plus Zero (Plus (Var ()) One))))) One) (Plus (Times (Times Zero (Times One (Var ()))) Zero) (Plus (Times One (Plus (Times (Times (Var ()) (Times (Plus (Times Zero One) One) (Plus (Times Zero (Plus (Times Zero (Times (Var ()) (Var ()))) (Times One (Var ())))) (Plus One (Times (Var ()) (Plus (Times (Var ()) (Var ())) (Plus (Var ()) (Var ())))))))) (Plus One (Plus (Times One (Times (Plus (Var ()) Zero) (Times (Var ()) One))) Zero))) (Plus (Var ()) (Plus One (Plus One Zero))))) (Plus (Times (Times (Var ()) (Times (Plus (Times (Var ()) (Plus (Times Zero (Var ())) (Times One (Plus (Times (Var ()) (Var ())) (Plus (Var ()) (Var ())))))) (Plus (Times One Zero) (Times (Times (Plus (Plus (Var ()) (Var ())) Zero) (Plus (Var ()) (Times (Var ()) (Var ())))) (Times (Var ()) (Plus (Plus (Var ()) (Var ())) (Var ())))))) (Plus (Plus Zero (Times One (Times (Var ()) (Plus (Plus (Var ()) (Var ())) One)))) (Times (Var ()) (Times Zero (Plus (Var ()) (Times One (Plus (Var ()) (Var ()))))))))) (Times One (Times (Plus (Times (Var ()) Zero) (Plus Zero One)) (Plus (Times (Plus (Times One Zero) (Var ())) (Plus (Var ()) (Plus (Plus (Times (Var ()) (Var ())) (Var ())) (Plus (Var ()) (Times (Var ()) (Var ())))))) Zero)))) (Var ()))))) (Times (Plus One (Plus One (Plus (Plus (Times (Plus (Plus (Times (Plus (Plus One (Times (Times (Var ()) (Var ())) (Times (Var ()) (Var ())))) (Var ())) (Var ())) One) Zero) (Var ())) (Plus (Times (Times (Times (Plus (Times Zero (Var ())) (Plus (Times (Plus (Var ()) (Var ())) One) (Var ()))) (Plus (Var ()) One)) (Plus (Times (Times One Zero) One) (Times (Times (Plus (Plus (Var ()) (Var ())) (Plus (Var ()) (Var ()))) (Times (Times (Var ()) (Var ())) (Times (Var ()) (Var ())))) (Plus (Plus Zero (Plus (Var ()) (Var ()))) One)))) (Times (Plus (Plus (Plus (Times (Plus (Var ()) (Var ())) One) Zero) (Times (Plus (Var ()) (Plus (Var ()) (Var ()))) (Times (Times (Var ()) (Var ())) (Plus (Var ()) (Var ()))))) (Var ())) Zero)) (Times (Var ()) (Times Zero (Times (Times (Times (Plus (Plus (Var ()) (Var ())) (Plus (Var ()) (Var ()))) (Plus (Plus (Var ()) (Var ())) (Times (Var ()) (Var ())))) (Plus (Var ()) (Var ()))) (Plus (Plus (Plus (Times (Var ()) (Var ())) (Times (Var ()) (Var ()))) (Times (Plus (Var ()) (Var ())) (Times (Var ()) (Var ())))) (Plus Zero (Var ())))))))) (Plus (Plus (Plus Zero (Plus One One)) (Plus (Times (Plus (Times One (Plus One (Plus (Times (Var ()) (Var ())) One))) Zero) (Plus (Plus (Var ()) (Times (Var ()) (Times (Plus (Var ()) (Var ())) One))) (Times (Plus (Plus (Times (Var ()) (Var ())) (Times (Var ()) (Var ()))) Zero) One))) (Times (Plus (Var ()) (Times (Plus One One) (Times (Var ()) (Times (Times (Var ()) (Var ())) (Plus (Var ()) (Var ())))))) (Times (Plus (Var ()) (Var ())) (Plus (Plus (Times (Plus (Var ()) (Var ())) (Times (Var ()) (Var ()))) (Times (Times (Var ()) (Var ())) (Plus (Var ()) (Var ())))) (Plus One (Plus (Var ()) (Times (Var ()) (Var ()))))))))) One)))) (Times (Times (Var ()) (Var ())) One))
exampleLargeStaged :: Expr (SpliceQ ())
exampleLargeStaged = Times (Plus (Times (Times (Times (Plus (Plus (Plus (Plus (Plus (Times (Plus One (Plus (Plus (Var [||()||]) (Var [||()||])) (Times (Var [||()||]) (Var [||()||])))) (Times One (Var [||()||]))) Zero) (Var [||()||])) Zero) (Times (Times (Plus One (Var [||()||])) (Plus (Times (Times (Plus One (Var [||()||])) (Var [||()||])) (Var [||()||])) (Plus (Plus (Var [||()||]) (Plus (Times (Var [||()||]) (Var [||()||])) (Times (Var [||()||]) (Var [||()||])))) (Plus (Plus (Plus (Var [||()||]) (Var [||()||])) (Times (Var [||()||]) (Var [||()||]))) (Times Zero One))))) One)) (Times (Plus (Var [||()||]) (Var [||()||])) (Times (Times (Var [||()||]) (Times (Times (Times (Times (Plus (Var [||()||]) (Var [||()||])) (Var [||()||])) (Times (Times (Var [||()||]) (Var [||()||])) (Plus (Var [||()||]) (Var [||()||])))) (Var [||()||])) (Times (Times One (Var [||()||])) One))) (Var [||()||])))) (Times (Plus (Times (Times Zero (Times (Times (Times One One) (Times One (Times (Times (Var [||()||]) (Var [||()||])) (Times (Var [||()||]) (Var [||()||]))))) (Times One Zero))) One) Zero) (Plus (Plus (Plus Zero One) (Var [||()||])) (Times Zero (Var [||()||]))))) (Plus (Plus Zero (Var [||()||])) (Plus (Times (Times (Plus One (Plus (Plus (Plus (Plus Zero (Var [||()||])) (Times (Plus (Var [||()||]) (Var [||()||])) (Var [||()||]))) (Times (Plus (Var [||()||]) (Times (Var [||()||]) (Var [||()||]))) (Times (Plus (Var [||()||]) (Var [||()||])) (Times (Var [||()||]) (Var [||()||]))))) (Times (Times One (Times (Times (Var [||()||]) (Var [||()||])) (Var [||()||]))) One))) (Times (Var [||()||]) (Plus (Plus (Times One (Times One (Plus (Var [||()||]) (Var [||()||])))) (Plus (Plus (Var [||()||]) (Plus (Var [||()||]) (Var [||()||]))) Zero)) One))) One) (Plus Zero (Plus (Var [||()||]) One))))) One) (Plus (Times (Times Zero (Times One (Var [||()||]))) Zero) (Plus (Times One (Plus (Times (Times (Var [||()||]) (Times (Plus (Times Zero One) One) (Plus (Times Zero (Plus (Times Zero (Times (Var [||()||]) (Var [||()||]))) (Times One (Var [||()||])))) (Plus One (Times (Var [||()||]) (Plus (Times (Var [||()||]) (Var [||()||])) (Plus (Var [||()||]) (Var [||()||])))))))) (Plus One (Plus (Times One (Times (Plus (Var [||()||]) Zero) (Times (Var [||()||]) One))) Zero))) (Plus (Var [||()||]) (Plus One (Plus One Zero))))) (Plus (Times (Times (Var [||()||]) (Times (Plus (Times (Var [||()||]) (Plus (Times Zero (Var [||()||])) (Times One (Plus (Times (Var [||()||]) (Var [||()||])) (Plus (Var [||()||]) (Var [||()||])))))) (Plus (Times One Zero) (Times (Times (Plus (Plus (Var [||()||]) (Var [||()||])) Zero) (Plus (Var [||()||]) (Times (Var [||()||]) (Var [||()||])))) (Times (Var [||()||]) (Plus (Plus (Var [||()||]) (Var [||()||])) (Var [||()||])))))) (Plus (Plus Zero (Times One (Times (Var [||()||]) (Plus (Plus (Var [||()||]) (Var [||()||])) One)))) (Times (Var [||()||]) (Times Zero (Plus (Var [||()||]) (Times One (Plus (Var [||()||]) (Var [||()||]))))))))) (Times One (Times (Plus (Times (Var [||()||]) Zero) (Plus Zero One)) (Plus (Times (Plus (Times One Zero) (Var [||()||])) (Plus (Var [||()||]) (Plus (Plus (Times (Var [||()||]) (Var [||()||])) (Var [||()||])) (Plus (Var [||()||]) (Times (Var [||()||]) (Var [||()||])))))) Zero)))) (Var [||()||]))))) (Times (Plus One (Plus One (Plus (Plus (Times (Plus (Plus (Times (Plus (Plus One (Times (Times (Var [||()||]) (Var [||()||])) (Times (Var [||()||]) (Var [||()||])))) (Var [||()||])) (Var [||()||])) One) Zero) (Var [||()||])) (Plus (Times (Times (Times (Plus (Times Zero (Var [||()||])) (Plus (Times (Plus (Var [||()||]) (Var [||()||])) One) (Var [||()||]))) (Plus (Var [||()||]) One)) (Plus (Times (Times One Zero) One) (Times (Times (Plus (Plus (Var [||()||]) (Var [||()||])) (Plus (Var [||()||]) (Var [||()||]))) (Times (Times (Var [||()||]) (Var [||()||])) (Times (Var [||()||]) (Var [||()||])))) (Plus (Plus Zero (Plus (Var [||()||]) (Var [||()||]))) One)))) (Times (Plus (Plus (Plus (Times (Plus (Var [||()||]) (Var [||()||])) One) Zero) (Times (Plus (Var [||()||]) (Plus (Var [||()||]) (Var [||()||]))) (Times (Times (Var [||()||]) (Var [||()||])) (Plus (Var [||()||]) (Var [||()||]))))) (Var [||()||])) Zero)) (Times (Var [||()||]) (Times Zero (Times (Times (Times (Plus (Plus (Var [||()||]) (Var [||()||])) (Plus (Var [||()||]) (Var [||()||]))) (Plus (Plus (Var [||()||]) (Var [||()||])) (Times (Var [||()||]) (Var [||()||])))) (Plus (Var [||()||]) (Var [||()||]))) (Plus (Plus (Plus (Times (Var [||()||]) (Var [||()||])) (Times (Var [||()||]) (Var [||()||]))) (Times (Plus (Var [||()||]) (Var [||()||])) (Times (Var [||()||]) (Var [||()||])))) (Plus Zero (Var [||()||])))))))) (Plus (Plus (Plus Zero (Plus One One)) (Plus (Times (Plus (Times One (Plus One (Plus (Times (Var [||()||]) (Var [||()||])) One))) Zero) (Plus (Plus (Var [||()||]) (Times (Var [||()||]) (Times (Plus (Var [||()||]) (Var [||()||])) One))) (Times (Plus (Plus (Times (Var [||()||]) (Var [||()||])) (Times (Var [||()||]) (Var [||()||]))) Zero) One))) (Times (Plus (Var [||()||]) (Times (Plus One One) (Times (Var [||()||]) (Times (Times (Var [||()||]) (Var [||()||])) (Plus (Var [||()||]) (Var [||()||])))))) (Times (Plus (Var [||()||]) (Var [||()||])) (Plus (Plus (Times (Plus (Var [||()||]) (Var [||()||])) (Times (Var [||()||]) (Var [||()||]))) (Times (Times (Var [||()||]) (Var [||()||])) (Plus (Var [||()||]) (Var [||()||])))) (Plus One (Plus (Var [||()||]) (Times (Var [||()||]) (Var [||()||]))))))))) One)))) (Times (Times (Var [||()||]) (Var [||()||])) One))
