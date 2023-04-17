-- GeNeRaTeD fOr: /home/thomas/repos/plancomps/CBS-beta/Unstable-Funcons-beta/Computations/Normal/Indexing/Indexing.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Normal.Indexing.Indexing where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("initialise-index",NullaryFuncon stepInitialise_index),("allocate-index",StrictFuncon stepAllocate_index),("lookup-index",StrictFuncon stepLookup_index)]

initialise_index_ = FName "initialise-index"
stepInitialise_index = evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- getMutPatt "value-index" [VPWildCard] env
            putMutTerm "value-index" (TSeq []) env
            stepTermTo (TName "null-value") env

allocate_index_ fargs = FApp "allocate-index" (fargs)
stepAllocate_index fargs =
    evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "ground-values")] env
            env <- getMutPatt "value-index" [VPSeqVar "V*" StarOp] env
            putMutTerm "value-index" (TSeq [TVar "V*",TVar "V"]) env
            stepTermTo (TApp "length" [TVar "V*",TVar "V"]) env

lookup_index_ fargs = FApp "lookup-index" (fargs)
stepLookup_index fargs =
    evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "N") (TName "natural-numbers")] env
            env <- getMutPatt "value-index" [VPSeqVar "V*" StarOp] env
            putMutTerm "value-index" (TVar "V*") env
            stepTermTo (TApp "index" [TVar "N",TVar "V*"]) env