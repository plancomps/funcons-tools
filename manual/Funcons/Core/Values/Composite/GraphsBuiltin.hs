{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.GraphsBuiltin where

import Funcons.EDSL
import qualified Funcons.Operations as VAL 

library = libFromList [
    ("is-cyclic", fromValOp is_cyclic_ VAL.is_cyclic_)
  , ("topological-sort", fromValOp topological_sort_ VAL.topological_sort_)
  ]

is_cyclic_ = applyFuncon "is-cyclic"
topological_sort_ = applyFuncon "topological-sort"
