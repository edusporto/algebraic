{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}


module Experiments.Ad.Test where

import Language.Haskell.TH

import Data.Map
import Data.Array.IO
import Control.Monad.Reader

import Experiments.Ad.Expressions
import Experiments.Ad.Abstract
import Experiments.Ad.Forward
import Experiments.Ad.Reverse

-- >>> reverseADExtract (\X -> 5) example3
-- fromList [(X,170)]

-- example1 :: Int -> ExtractedStagedAD -> Map X Int
-- example1 f = $$([||\y -> $$(f (\x -> [||y||]) example1Staged) ||])

example11 :: Int -> Map X Int
example11 = $$([||\y -> $$(reverseADStagedExtract (\x -> [||y||]) example1Staged) ||])

example12 :: Int -> Map X Int
example12 = $$([||\y -> $$(reverseADEndoStagedExtract (\x -> [||y||]) example1Staged) ||])

example13 :: Int -> IO (Map X Int)
example13 = $$([||\y -> $$(reverseAD_CY_IO_Staged_Extract (\x -> [||y||]) example1Staged [||(X,X)||])||])

example11Expanded :: Int -> Map X Int
example11Expanded = \y
  -> (sparseSA . (absHom . eCW))
       ((CW y)
          (Hom (\ d -> Sparse ((singleton X) (SA d))))
          `times`
            ((CW y)
               (Hom (\d -> Sparse ((singleton X) (SA d))))
               `plus` one))

example12Expanded :: Int -> Map X Int
example12Expanded = \ y_aiakT
  -> (sparseSA . (absEndo . (absHom . eCW)))
       ((CW y_aiakT)
          (Hom
             (\ d_aiakU
                -> E (\ e_aiakV
                        -> Sparse
                             ((((insertWith plus) X) (SA d_aiakU)) (sparse e_aiakV)))))
          `times`
            ((CW y_aiakT)
               (Hom
                  (\ d_aiakW
                     -> E (\ e_aiakX
                             -> Sparse
                                  ((((insertWith plus) X) (SA d_aiakW)) (sparse e_aiakX)))))
               `plus` one))

example13Expanded :: Int -> IO (Map X Int)
example13Expanded = \ y_a54Ag
  -> do arr_a54Ah :: IOArray v (SemiringAsAlgebra d) <- (newArray
                                                           (X, X))
                                                          zero
        (runReaderT
           (sm
              ((absHom . eCW)
                 ((CW y_a54Ag)
                    (Hom
                       (\ d_a54Aj -> (SM $ (modifyArrayAt (`mappend` shom d_a54Aj)) X)))
                    `times`
                      ((CW y_a54Ag)
                         (Hom
                            (\ d_a54Ak -> (SM $ (modifyArrayAt (`mappend` shom d_a54Ak)) X)))
                         `plus` one)))))
          arr_a54Ah
        m_a54Ai <- getAssocs arr_a54Ah
        (return $ (Data.Map.map sa $ fromAscList m_a54Ai))

example11ExpandedClean :: Int -> Map X Int
example11ExpandedClean y = (sparseSA . (absHom . eCW))
       (CW y
          (Hom (Sparse . singleton X . SA))
            `times`
            (CW y
               (Hom (Sparse . singleton X . SA))
               `plus` one))

example12ExpandedClean :: Int -> Map X Int
example12ExpandedClean y = (sparseSA . (absEndo . (absHom . eCW)))
      (CW y
         (Hom (\d -> E (Sparse . insertWith plus X (SA d) . sparse)))
         `times`
           (CW y
              (Hom (\d -> E (Sparse . insertWith plus X (SA d) . sparse)))
              `plus` one))

example13ExpandedClean :: Int -> IO (Map X Int)
example13ExpandedClean y = do
   arr :: IOArray v (SemiringAsAlgebra d) <- newArray (X, X) zero
   runReaderT (sm ((absHom . eCW)
      (CW y
         (Hom (\d -> SM $ modifyArrayAt (`mappend` shom d) X))
         `times`
            (CW y
               (Hom (\d -> SM $ modifyArrayAt (`mappend` shom d) X))
               `plus` one))))
      arr
   m <- getAssocs arr
   return $ Data.Map.map sa (fromAscList m)
