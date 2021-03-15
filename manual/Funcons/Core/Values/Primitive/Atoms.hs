{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Primitive.Atoms where

import Funcons.EDSL hiding (atom_)
import qualified Funcons.Operations as VAL

library = libFromList [
        ("atom", fromValOp atom_ VAL.atom_)
    ,   ("atoms", fromValOp atoms_ VAL.atoms_) 
    ]

atoms_ = applyFuncon "atoms"
atom_ = applyFuncon "atom"

