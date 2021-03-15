-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Abstraction/Functions/Functions.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Abstraction.Functions.Functions where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("functions",DataTypeMemberss "functions" [TPVar "T",TPVar "T'"] [DataTypeMemberConstructor "function" [TApp "abstractions" [TSortComputesFrom (TVar "T") (TVar "T'")]] (Just [TPVar "T",TPVar "T'"])])]

funcons = libFromList
    [("function",StrictFuncon stepFunction),("apply",StrictFuncon stepApply),("supply",StrictFuncon stepSupply),("compose",StrictFuncon stepCompose),("uncurry",StrictFuncon stepUncurry),("curry",StrictFuncon stepCurry),("partial-apply",StrictFuncon stepPartial_apply),("functions",StrictFuncon stepFunctions)]

function_ fargs = FApp "function" (fargs)
stepFunction fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 102)]),FValue (ADTVal "unicode-character" [FValue (Int 117)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 110)])])),TVar "_X1"]) env

apply_ fargs = FApp "apply" (fargs)
stepApply fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "function" [PADT "abstraction" [VPMetaVar "X"]],VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TApp "give" [TVar "V",TVar "X"]) env

supply_ fargs = FApp "supply" (fargs)
stepSupply fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "function" [PADT "abstraction" [VPMetaVar "X"]],VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TApp "thunk" [TApp "abstraction" [TApp "give" [TVar "V",TVar "X"]]]) env

compose_ fargs = FApp "compose" (fargs)
stepCompose fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "function" [PADT "abstraction" [VPMetaVar "Y"]],PADT "function" [PADT "abstraction" [VPMetaVar "X"]]] env
            rewriteTermTo (TApp "function" [TApp "abstraction" [TApp "give" [TVar "X",TVar "Y"]]]) env

uncurry_ fargs = FApp "uncurry" (fargs)
stepUncurry fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "F") (TApp "functions" [TName "values",TApp "functions" [TName "values",TName "values"]])] env
            rewriteTermTo (TApp "function" [TApp "abstraction" [TApp "apply" [TApp "apply" [TVar "F",TApp "checked" [TApp "index" [TFuncon (FValue (Nat 1)),TApp "tuple-elements" [TName "given"]]]],TApp "checked" [TApp "index" [TFuncon (FValue (Nat 2)),TApp "tuple-elements" [TName "given"]]]]]]) env

curry_ fargs = FApp "curry" (fargs)
stepCurry fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "F") (TApp "functions" [TApp "tuples" [TName "values",TName "values"],TName "values"])] env
            rewriteTermTo (TApp "function" [TApp "abstraction" [TApp "partial-apply" [TVar "F",TName "given"]]]) env

partial_apply_ fargs = FApp "partial-apply" (fargs)
stepPartial_apply fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "F") (TApp "functions" [TApp "tuples" [TName "values",TName "values"],TName "values"]),VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TApp "function" [TApp "abstraction" [TApp "apply" [TVar "F",TApp "tuple" [TVar "V",TName "given"]]]]) env

functions_ = FApp "functions"
stepFunctions ts = rewriteType "functions" ts