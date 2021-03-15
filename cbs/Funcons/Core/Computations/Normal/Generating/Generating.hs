-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Computations/Normal/Generating/Generating.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Normal.Generating.Generating where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("fresh-atom",NullaryFuncon stepFresh_atom),("use-atom-not-in",StrictFuncon stepUse_atom_not_in)]

fresh_atom_ = FName "fresh-atom"
stepFresh_atom = evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- getMutPatt "used-atom-set" (VPMetaVar "SA") env
            env <- lifted_sideCondition (SCPatternMatch (TApp "element-not-in" [TName "atoms",TVar "SA"]) [VPMetaVar "A"]) env
            putMutTerm "used-atom-set" (TApp "set-insert" [TVar "A",TVar "SA"]) env
            stepTermTo (TVar "A") env

use_atom_not_in_ fargs = FApp "use-atom-not-in" (fargs)
stepUse_atom_not_in fargs =
    evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "SA") (TApp "sets" [TName "atoms"])] env
            env <- getMutPatt "used-atom-set" (VPMetaVar "SA'") env
            env <- lifted_sideCondition (SCPatternMatch (TApp "element-not-in" [TName "atoms",TVar "SA"]) [VPMetaVar "A"]) env
            putMutTerm "used-atom-set" (TApp "set-insert" [TVar "A",TVar "SA'"]) env
            stepTermTo (TVar "A") env