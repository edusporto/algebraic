{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

import Language.Haskell.TH

import TuplesTH
import Dec

import Hutton
import Data.Functor.Identity

$someSplice

-- $(generateTupleClass 3)
-- $(generateTupleInstance 3 5)
-- $(generateTupleBoilerplate 62)

n = $$(eval2 (Add (Val 1) (Val 2)))
-- expands to
-- (1 + 2)

elim :: Identity Int
elim = $$(elimApplicative $$(elimExpr (Add (Val 1) (Val 2))) identityDict)
-- expands to
-- (idAp ((idAp (Identity (+))) (Identity 1))) (Identity 2)

