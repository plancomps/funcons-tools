-- GeNeRaTeD fOr: /home/thomas/repos/plancomps/CBS-beta/Unstable-Funcons-beta/Computations/Normal/Memos/Memos.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Normal.Memos.Memos where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("initialise-memos",NonStrictFuncon stepInitialise_memos),("memo-value",PartiallyStrictFuncon [Strict,NonStrict] NonStrict stepMemo_value),("initialise-memo-value",StrictFuncon stepInitialise_memo_value),("memo-value-recall",StrictFuncon stepMemo_value_recall)]

initialise_memos_ fargs = FApp "initialise-memos" (fargs)
stepInitialise_memos fargs =
    evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getMutPatt "memo-map" [VPWildCard] env
            putMutTerm "memo-map" (TApp "map" []) env
            stepTermTo (TVar "X") env

memo_value_ fargs = FApp "memo-value" (fargs)
stepMemo_value fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "K") (TName "ground-values"),PMetaVar "X"] env
            rewriteTermTo (TApp "else" [TApp "memo-value-recall" [TVar "K"],TApp "give" [TVar "X",TApp "sequential" [TApp "else" [TApp "initialise-memo-value" [TVar "K",TName "given"],TName "null-value"],TApp "memo-value-recall" [TVar "K"]]]]) env

initialise_memo_value_ fargs = FApp "initialise-memo-value" (fargs)
stepInitialise_memo_value fargs =
    evalRules [] [step1,step2]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "K") (TName "ground-values"),VPAnnotated (VPMetaVar "V") (TName "values")] env
            env <- getMutPatt "memo-map" [VPMetaVar "M"] env
            env <- lifted_sideCondition (SCPatternMatch (TApp "map-unite" [TVar "M",TMap [TBinding (TVar "K") (TVar "V")]]) [VPMetaVar "M'"]) env
            putMutTerm "memo-map" (TVar "M'") env
            stepTermTo (TName "null-value") env
          step2 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "K") (TName "ground-values"),VPAnnotated (VPMetaVar "V") (TName "values")] env
            env <- getMutPatt "memo-map" [VPMetaVar "M"] env
            env <- lifted_sideCondition (SCPatternMatch (TApp "map-unite" [TVar "M",TMap [TBinding (TVar "K") (TVar "V")]]) []) env
            putMutTerm "memo-map" (TVar "M") env
            stepTermTo (TName "fail") env

memo_value_recall_ fargs = FApp "memo-value-recall" (fargs)
stepMemo_value_recall fargs =
    evalRules [] [step1,step2]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "K") (TName "ground-values")] env
            env <- getMutPatt "memo-map" [VPMetaVar "M"] env
            env <- lifted_sideCondition (SCPatternMatch (TApp "lookup" [TVar "M",TVar "K"]) [VPMetaVar "V"]) env
            putMutTerm "memo-map" (TVar "M") env
            stepTermTo (TVar "V") env
          step2 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "K") (TName "ground-values")] env
            env <- getMutPatt "memo-map" [VPMetaVar "M"] env
            env <- lifted_sideCondition (SCPatternMatch (TApp "lookup" [TVar "M",TVar "K"]) []) env
            putMutTerm "memo-map" (TVar "M") env
            stepTermTo (TName "fail") env