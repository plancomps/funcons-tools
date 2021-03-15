{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Primitive.BoolBuiltin where

import Funcons.EDSL
import Funcons.Operations hiding (Values, libFromList)

library = libFromList []

bool_ :: Bool -> Funcons 
bool_ = FValue . tobool
