-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Abstraction/Thunks/Thunks.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Abstraction.Thunks.Thunks where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("thunks",DataTypeMemberss "thunks" [TPVar "T"] [DataTypeMemberConstructor "thunk" [TApp "abstractions" [TSortComputesFrom (TSeq []) (TVar "T")]] (Just [TPVar "T"])])]

funcons = libFromList
    [("thunk",StrictFuncon stepThunk),("force",StrictFuncon stepForce),("thunks",StrictFuncon stepThunks)]

thunk_ fargs = FApp "thunk" (fargs)
stepThunk fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 104)]),FValue (ADTVal "unicode-character" [FValue (Int 117)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 107)])])),TVar "_X1"]) env

force_ fargs = FApp "force" (fargs)
stepForce fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "thunk" [PADT "abstraction" [VPMetaVar "X"]]] env
            rewriteTermTo (TApp "no-given" [TVar "X"]) env

thunks_ = FApp "thunks"
stepThunks ts = rewriteType "thunks" ts