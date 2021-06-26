{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Primitive.StringsBuiltin where

import Funcons.EDSL
import qualified Funcons.Operations as VAL

library = libFromList [
        ("to-string", fromValOp to_string_ VAL.to_string_)
    ]

to_string_ = applyFuncon "to-string"

