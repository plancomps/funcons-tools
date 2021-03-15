{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Funcons.Core.Values.TypesBuiltin where

import Funcons.EDSL 
import qualified Funcons.Operations as VAL

library = libFromList [
    ("datatype-values", fromNullaryValOp datatype_values_ VAL.datatype_values_)
  , ("ground-values", fromNullaryValOp ground_values_ VAL.ground_values_)
  , ("ground-vals", fromNullaryValOp ground_values_ VAL.ground_values_)
  , ("types", fromNullaryValOp types_ VAL.types_)
  , ("value-types", fromNullaryValOp value_types_ VAL.value_types_)
  , ("empty-type", fromNullaryValOp empty_type_ VAL.empty_type_)
  ]

datatype_values_ = applyFuncon "datatype-values" 
ground_values_ = applyFuncon "ground-values"
ground_vals_ = applyFuncon "ground-vals"
types_ = applyFuncon "types"
value_types_ = applyFuncon "value-types"
empty_type_ = applyFuncon "empty-type"
