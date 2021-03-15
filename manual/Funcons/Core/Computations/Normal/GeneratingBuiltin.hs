{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Normal.GeneratingBuiltin where

import Funcons.EDSL
import Funcons.Operations hiding (Values,tobool,libFromList)

import Data.Set (fromList)

library = libFromList [ 
        ("initialise-generating", NonStrictFuncon stepInitialise_generating)
    ]

initialise_generating_ = FName "initialise-generating"
stepInitialise_generating fargs =
    evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "P"] env
            env <- getMutPatt "used-atom-set" (VPWildCard) env
            putMut "used-atom-set" reserved_atoms
            stepTermTo (TVar "P") env
          reserved_atoms = Set (fromList [])

