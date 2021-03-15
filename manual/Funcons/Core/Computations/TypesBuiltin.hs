{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.TypesBuiltin where

import Funcons.EDSL
import Funcons.Types
import Funcons.Operations (Types(ComputationTypes))

library = libFromList [
    ("computation-types", NullaryFuncon stepComputation_Types)
  ]

computation_types_ = applyFuncon "computation-types"
stepComputation_Types = rewritten $ typeVal ComputationTypes
