{-# LANGUAGE TemplateHaskell #-}

module Dec where

import Language.Haskell.TH

someSplice :: Q [Dec]
someSplice = [d|y = 0|]
