-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Composite/Sequences/Sequences.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.Sequences.Sequences where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("length",StrictFuncon stepLength),("is-in",StrictFuncon stepIs_in),("index",StrictFuncon stepIndex),("first",StrictFuncon stepFirst),("second",StrictFuncon stepSecond),("third",StrictFuncon stepThird),("first-n",StrictFuncon stepFirst_n),("drop-first-n",StrictFuncon stepDrop_first_n),("reverse",StrictFuncon stepReverse),("n-of",StrictFuncon stepN_of),("intersperse",StrictFuncon stepIntersperse)]

length_ fargs = FApp "length" (fargs)
stepLength fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [] env
            rewriteTermTo (TFuncon (FValue (Nat 0))) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TApp "natural-successor" [TApp "length" [TVar "V*"]]) env

is_in_ fargs = FApp "is-in" (fargs)
stepIs_in fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPMetaVar "V'") (TName "values"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TApp "or" [TApp "is-equal" [TVar "V",TVar "V'"],TApp "is-in" [TVar "V",TVar "V*"]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TName "false") env

index_ fargs = FApp "index" (fargs)
stepIndex fargs =
    evalRules [rewrite1,rewrite2,rewrite3,rewrite4] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPLit (Nat 1),VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TVar "V") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "N") (TName "positive-integers"),VPAnnotated VPWildCard (TName "values"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            env <- sideCondition (SCPatternMatch (TApp "natural-predecessor" [TVar "N"]) [VPMetaVar "N'"]) env
            rewriteTermTo (TApp "index" [TVar "N'",TVar "V*"]) env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPLit (Nat 0),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TSeq []) env
          rewrite4 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated VPWildCard (TName "positive-integers")] env
            rewriteTermTo (TSeq []) env

first_ fargs = FApp "first" (fargs)
stepFirst fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TVar "V") env

second_ fargs = FApp "second" (fargs)
stepSecond fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated VPWildCard (TName "values"),VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TVar "V") env

third_ fargs = FApp "third" (fargs)
stepThird fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated VPWildCard (TName "values"),VPAnnotated VPWildCard (TName "values"),VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TVar "V") env

first_n_ fargs = FApp "first-n" (fargs)
stepFirst_n fargs =
    evalRules [rewrite1,rewrite2,rewrite3] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPLit (Nat 0),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TSeq []) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "N") (TName "positive-integers"),VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            env <- sideCondition (SCPatternMatch (TApp "natural-predecessor" [TVar "N"]) [VPMetaVar "N'"]) env
            rewriteTermTo (TSeq [TVar "V",TApp "first-n" [TVar "N'",TVar "V*"]]) env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "N") (TName "positive-integers")] env
            rewriteTermTo (TSeq []) env

drop_first_n_ fargs = FApp "drop-first-n" (fargs)
stepDrop_first_n fargs =
    evalRules [rewrite1,rewrite2,rewrite3] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPLit (Nat 0),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TVar "V*") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "N") (TName "positive-integers"),VPAnnotated VPWildCard (TName "values"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            env <- sideCondition (SCPatternMatch (TApp "natural-predecessor" [TVar "N"]) [VPMetaVar "N'"]) env
            rewriteTermTo (TApp "drop-first-n" [TVar "N'",TVar "V*"]) env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "N") (TName "positive-integers")] env
            rewriteTermTo (TSeq []) env

reverse_ fargs = FApp "reverse" (fargs)
stepReverse fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [] env
            rewriteTermTo (TSeq []) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TSeq [TApp "reverse" [TVar "V*"],TVar "V"]) env

n_of_ fargs = FApp "n-of" (fargs)
stepN_of fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPLit (Nat 0),VPAnnotated VPWildCard (TName "values")] env
            rewriteTermTo (TSeq []) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "N") (TName "positive-integers"),VPAnnotated (VPMetaVar "V") (TName "values")] env
            env <- sideCondition (SCPatternMatch (TApp "natural-predecessor" [TVar "N"]) [VPMetaVar "N'"]) env
            rewriteTermTo (TSeq [TVar "V",TApp "n-of" [TVar "N'",TVar "V"]]) env

intersperse_ fargs = FApp "intersperse" (fargs)
stepIntersperse fargs =
    evalRules [rewrite1,rewrite2,rewrite3] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated VPWildCard (TName "values")] env
            rewriteTermTo (TSeq []) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated VPWildCard (TName "values"),VPMetaVar "V"] env
            rewriteTermTo (TVar "V") env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V'") (TName "values"),VPAnnotated (VPMetaVar "V1") (TName "values"),VPAnnotated (VPMetaVar "V2") (TName "values"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TSeq [TVar "V1",TVar "V'",TApp "intersperse" [TVar "V'",TVar "V2",TVar "V*"]]) env