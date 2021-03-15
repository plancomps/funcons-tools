-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Composite/Trees/Trees.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.Trees.Trees where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("trees",DataTypeMemberss "trees" [TPVar "T"] [DataTypeMemberConstructor "tree" [TVar "T",TSortSeq (TApp "trees" [TVar "T"]) StarOp] (Just [TPVar "T"])])]

funcons = libFromList
    [("tree",StrictFuncon stepTree),("tree-root-value",StrictFuncon stepTree_root_value),("tree-branch-sequence",StrictFuncon stepTree_branch_sequence),("single-branching-sequence",StrictFuncon stepSingle_branching_sequence),("forest-root-value-sequence",StrictFuncon stepForest_root_value_sequence),("forest-branch-sequence",StrictFuncon stepForest_branch_sequence),("forest-value-sequence",StrictFuncon stepForest_value_sequence),("trees",StrictFuncon stepTrees)]

tree_ fargs = FApp "tree" (fargs)
stepTree fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1",VPSeqVar "_X2*" StarOp] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X2*") (TSortSeq (TSortSeq (TName "values") QuestionMarkOp) StarOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 101)])])),TVar "_X1",TVar "_X2*"]) env

tree_root_value_ fargs = FApp "tree-root-value" (fargs)
stepTree_root_value fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "tree" [VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPSeqVar "___" StarOp) (TSortSeq (TApp "trees" [TName "values"]) StarOp)]] env
            rewriteTermTo (TVar "V") env

tree_branch_sequence_ fargs = FApp "tree-branch-sequence" (fargs)
stepTree_branch_sequence fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "tree" [VPAnnotated VPWildCard (TName "values"),VPAnnotated (VPSeqVar "B*" StarOp) (TSortSeq (TApp "trees" [TName "values"]) StarOp)]] env
            rewriteTermTo (TVar "B*") env

single_branching_sequence_ fargs = FApp "single-branching-sequence" (fargs)
stepSingle_branching_sequence fargs =
    evalRules [rewrite1,rewrite2,rewrite3] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "tree" [VPAnnotated (VPMetaVar "V") (TName "values")]] env
            rewriteTermTo (TVar "V") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "tree" [VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPMetaVar "B") (TApp "trees" [TName "values"])]] env
            rewriteTermTo (TApp "left-to-right" [TVar "V",TApp "single-branching-sequence" [TVar "B"]]) env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "tree" [VPAnnotated VPWildCard (TName "values"),VPAnnotated VPWildCard (TApp "trees" [TName "values"]),VPAnnotated (VPSeqVar "___" PlusOp) (TSortSeq (TApp "trees" [TName "values"]) PlusOp)]] env
            rewriteTermTo (TName "fail") env

forest_root_value_sequence_ fargs = FApp "forest-root-value-sequence" (fargs)
stepForest_root_value_sequence fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "B") (TApp "trees" [TName "values"]),VPAnnotated (VPSeqVar "B*" StarOp) (TSortSeq (TApp "trees" [TName "values"]) StarOp)] env
            rewriteTermTo (TSeq [TApp "tree-root-value" [TVar "B"],TApp "forest-root-value-sequence" [TVar "B*"]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [] env
            rewriteTermTo (TSeq []) env

forest_branch_sequence_ fargs = FApp "forest-branch-sequence" (fargs)
stepForest_branch_sequence fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "B") (TApp "trees" [TName "values"]),VPAnnotated (VPSeqVar "B*" StarOp) (TSortSeq (TApp "trees" [TName "values"]) StarOp)] env
            rewriteTermTo (TSeq [TApp "tree-branch-sequence" [TVar "B"],TApp "forest-branch-sequence" [TVar "B*"]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [] env
            rewriteTermTo (TSeq []) env

forest_value_sequence_ fargs = FApp "forest-value-sequence" (fargs)
stepForest_value_sequence fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "tree" [VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPSeqVar "B1*" StarOp) (TSortSeq (TApp "trees" [TName "values"]) StarOp)],VPAnnotated (VPSeqVar "B2*" StarOp) (TSortSeq (TApp "trees" [TName "values"]) StarOp)] env
            rewriteTermTo (TSeq [TVar "V",TApp "forest-value-sequence" [TVar "B1*"],TApp "forest-value-sequence" [TVar "B2*"]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [] env
            rewriteTermTo (TSeq []) env

trees_ = FApp "trees"
stepTrees ts = rewriteType "trees" ts