-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Value-Types/Value-Types.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.ValueTypes.ValueTypes where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("is-in-type",StrictFuncon stepIs_in_type),("is",StrictFuncon stepIs_in_type),("is-value",StrictFuncon stepIs_value),("is-val",StrictFuncon stepIs_value),("when-true",StrictFuncon stepWhen_true),("when",StrictFuncon stepWhen_true),("cast-to-type",StrictFuncon stepCast_to_type),("cast",StrictFuncon stepCast_to_type),("is-equal",StrictFuncon stepIs_equal),("is-eq",StrictFuncon stepIs_equal)]

is_in_type_ fargs = FApp "is-in-type" (fargs)
is_ fargs = FApp "is-in-type" (fargs)
stepIs_in_type fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPMetaVar "T") (TName "types")] env
            env <- sideCondition (SCIsInSort (TVar "V") (TVar "T")) env
            rewriteTermTo (TName "true") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPMetaVar "T") (TName "types")] env
            env <- sideCondition (SCIsInSort (TVar "V") (TSortComplement (TVar "T"))) env
            rewriteTermTo (TName "false") env

is_value_ fargs = FApp "is-value" (fargs)
is_val_ fargs = FApp "is-value" (fargs)
stepIs_value fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated VPWildCard (TName "values")] env
            rewriteTermTo (TName "true") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [] env
            rewriteTermTo (TName "false") env

when_true_ fargs = FApp "when-true" (fargs)
when_ fargs = FApp "when-true" (fargs)
stepWhen_true fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "true" [],VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TVar "V") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "false" [],VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TSeq []) env

cast_to_type_ fargs = FApp "cast-to-type" (fargs)
cast_ fargs = FApp "cast-to-type" (fargs)
stepCast_to_type fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPMetaVar "T") (TName "types")] env
            env <- sideCondition (SCIsInSort (TVar "V") (TVar "T")) env
            rewriteTermTo (TVar "V") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPMetaVar "T") (TName "types")] env
            env <- sideCondition (SCIsInSort (TVar "V") (TSortComplement (TVar "T"))) env
            rewriteTermTo (TSeq []) env

is_equal_ fargs = FApp "is-equal" (fargs)
is_eq_ fargs = FApp "is-equal" (fargs)
stepIs_equal fargs =
    evalRules [rewrite1,rewrite2,rewrite3,rewrite4] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "ground-values"),VPAnnotated (VPMetaVar "W") (TName "ground-values")] env
            env <- sideCondition (SCEquality (TVar "V") (TVar "W")) env
            rewriteTermTo (TName "true") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "ground-values"),VPAnnotated (VPMetaVar "W") (TName "ground-values")] env
            env <- sideCondition (SCInequality (TVar "V") (TVar "W")) env
            rewriteTermTo (TName "false") env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TSortComplement (TName "ground-values")),VPAnnotated (VPMetaVar "W") (TName "values")] env
            rewriteTermTo (TName "false") env
          rewrite4 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPMetaVar "W") (TSortComplement (TName "ground-values"))] env
            rewriteTermTo (TName "false") env