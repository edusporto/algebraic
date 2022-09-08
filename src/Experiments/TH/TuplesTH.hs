{-# LANGUAGE TemplateHaskellQuotes #-}

module Experiments.TH.TuplesTH where

import Control.Monad (unless)
import Data.Traversable (for)
import Language.Haskell.TH

-- >>> runQ [d|x :: (a,b,c); x = undefined|]
-- [SigD x_3 (AppT (AppT (AppT (TupleT 3) (VarT a_0)) (VarT b_1)) (VarT c_2)),ValD (VarP x_3) (NormalB (VarE GHC.Err.undefined)) []]

-- >>> runQ [d|x :: a -> b; x = undefined|]
-- [SigD x_6 (AppT (AppT ArrowT (VarT a_4)) (VarT b_5)),ValD (VarP x_6) (NormalB (VarE GHC.Err.undefined)) []]

generateTupleClass :: Int -> Q [Dec]
generateTupleClass size = do
  unless (size > 0) $
    fail $ "Non-positive size: " ++ size'
  pure [cDecl]
  where
    size' = show size
    className = mkName ("Tuple" ++ size')
    methodName = mkName ('_' : size')

    t = mkName "t"
    r = mkName "r"

    -- class TupleX t r | t -> r where
    cDecl = ClassD [] className [PlainTV t, PlainTV r] [FunDep [t] [r]] [mDecl]    -- GHC 8
    -- cDecl = ClassD [] className [PlainTV t (), PlainTV r ()] [FunDep [t] [r]] [mDecl] -- GHC 9

    -- _X :: t -> r
    mDecl = SigD methodName (AppT (AppT ArrowT (VarT t)) (VarT r))

generateTupleInstance :: Int -> Int -> Q [Dec]
generateTupleInstance element size = do
  unless (size > 0) $
    fail $ "Non-positive size: " ++ element'
  unless (size >= element) $
    fail $ "Can't extract element " ++ element' ++ " of " ++ size' ++ "-tuple"
  pure [iDecl]
  where
    element' = show element
    size' = show size
    className = mkName ("Tuple" ++ element')
    methodName = mkName ('_' : element')

    x = mkName "x"

    vars = [mkName ('t' : show n) | n <- [1..size]]

    signature = foldl (\acc var -> AppT acc (VarT var)) (TupleT size) vars

    -- instance TupleX (t1, ..., tX, ...) tX where
    iDecl = InstanceD Nothing [] (AppT (AppT (ConT className) signature) (VarT $ mkName ('t' : element'))) [mDecl]
    --   _X (_, _, ..., x, ...) = x
    mDecl = FunD methodName [Clause [TupP $ replicate (element - 1) WildP ++ [VarP x] ++ replicate (size - element) WildP] (NormalB $ VarE x) []]

generateTupleBoilerplate :: Int -> Q [Dec]
generateTupleBoilerplate size =
  concatFor [1..size] $ \classDeclIndex -> do
    cDecl <- generateTupleClass classDeclIndex
    iDecls <- for [1..classDeclIndex] $ \instanceDeclIndex ->
      generateTupleInstance instanceDeclIndex classDeclIndex

    pure $ concat (cDecl : iDecls)
  where
    concatFor xs = fmap concat . for xs
