-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Computations/Normal/Giving/Giving.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Normal.Giving.Giving where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("initialise-giving",NonStrictFuncon stepInitialise_giving),("give",PartiallyStrictFuncon [Strict,NonStrict] NonStrict stepGive),("given",NullaryFuncon stepGiven),("no-given",NonStrictFuncon stepNo_given),("left-to-right-map",PartiallyStrictFuncon [NonStrict] Strict stepLeft_to_right_map),("interleave-map",PartiallyStrictFuncon [NonStrict] Strict stepInterleave_map),("left-to-right-repeat",PartiallyStrictFuncon [NonStrict,Strict,Strict] NonStrict stepLeft_to_right_repeat),("interleave-repeat",PartiallyStrictFuncon [NonStrict,Strict,Strict] NonStrict stepInterleave_repeat),("left-to-right-filter",PartiallyStrictFuncon [NonStrict] Strict stepLeft_to_right_filter),("interleave-filter",PartiallyStrictFuncon [NonStrict] Strict stepInterleave_filter),("fold-left",PartiallyStrictFuncon [NonStrict,Strict] Strict stepFold_left),("fold-right",PartiallyStrictFuncon [NonStrict,Strict] Strict stepFold_right)]

initialise_giving_ fargs = FApp "initialise-giving" (fargs)
stepInitialise_giving fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "X"] env
            rewriteTermTo (TApp "no-given" [TVar "X"]) env

give_ fargs = FApp "give" (fargs)
stepGive fargs =
    evalRules [rewrite1] [step1]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated PWildCard (TName "values"),PAnnotated (PMetaVar "W") (TName "values")] env
            rewriteTermTo (TVar "W") env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PAnnotated (PMetaVar "V") (TName "values"),PMetaVar "Y"] env
            env <- getInhPatt "given-value" [VPSeqVar "___" QuestionMarkOp] env
            env <- withInhTerm "given-value" (TVar "V") env (premise (TVar "Y") [PMetaVar "Y'"] env)
            stepTermTo (TApp "give" [TVar "V",TVar "Y'"]) env

given_ = FName "given"
stepGiven = evalRules [] [step1,step2]
    where step1 = do
            let env = emptyEnv
            env <- getInhPatt "given-value" [VPAnnotated (VPMetaVar "V") (TName "values")] env
            stepTermTo (TVar "V") env
          step2 = do
            let env = emptyEnv
            env <- getInhPatt "given-value" [] env
            stepTermTo (TName "fail") env

no_given_ fargs = FApp "no-given" (fargs)
stepNo_given fargs =
    evalRules [rewrite1] [step1]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "U") (TName "values")] env
            rewriteTermTo (TVar "U") env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getInhPatt "given-value" [VPSeqVar "___" QuestionMarkOp] env
            env <- withInhTerm "given-value" (TSeq []) env (premise (TVar "X") [PMetaVar "X'"] env)
            stepTermTo (TApp "no-given" [TVar "X'"]) env

left_to_right_map_ fargs = FApp "left-to-right-map" (fargs)
stepLeft_to_right_map fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "F",PAnnotated (PMetaVar "V") (TName "values"),PAnnotated (PSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TApp "left-to-right" [TApp "give" [TVar "V",TVar "F"],TApp "left-to-right-map" [TVar "F",TVar "V*"]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- fsMatch fargs [PWildCard] env
            rewriteTermTo (TSeq []) env

interleave_map_ fargs = FApp "interleave-map" (fargs)
stepInterleave_map fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "F",PAnnotated (PMetaVar "V") (TName "values"),PAnnotated (PSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TApp "interleave" [TApp "give" [TVar "V",TVar "F"],TApp "interleave-map" [TVar "F",TVar "V*"]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- fsMatch fargs [PWildCard] env
            rewriteTermTo (TSeq []) env

left_to_right_repeat_ fargs = FApp "left-to-right-repeat" (fargs)
stepLeft_to_right_repeat fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "F",PAnnotated (PMetaVar "M") (TName "integers"),PAnnotated (PMetaVar "N") (TName "integers")] env
            env <- sideCondition (SCEquality (TApp "is-less-or-equal" [TVar "M",TVar "N"]) (TName "true")) env
            rewriteTermTo (TApp "left-to-right" [TApp "give" [TVar "M",TVar "F"],TApp "left-to-right-repeat" [TVar "F",TApp "int-add" [TVar "M",TFuncon (FValue (Nat 1))],TVar "N"]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- fsMatch fargs [PWildCard,PAnnotated (PMetaVar "M") (TName "integers"),PAnnotated (PMetaVar "N") (TName "integers")] env
            env <- sideCondition (SCEquality (TApp "is-less-or-equal" [TVar "M",TVar "N"]) (TName "false")) env
            rewriteTermTo (TSeq []) env

interleave_repeat_ fargs = FApp "interleave-repeat" (fargs)
stepInterleave_repeat fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "F",PAnnotated (PMetaVar "M") (TName "integers"),PAnnotated (PMetaVar "N") (TName "integers")] env
            env <- sideCondition (SCEquality (TApp "is-less-or-equal" [TVar "M",TVar "N"]) (TName "true")) env
            rewriteTermTo (TApp "interleave" [TApp "give" [TVar "M",TVar "F"],TApp "interleave-repeat" [TVar "F",TApp "int-add" [TVar "M",TFuncon (FValue (Nat 1))],TVar "N"]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- fsMatch fargs [PWildCard,PAnnotated (PMetaVar "M") (TName "integers"),PAnnotated (PMetaVar "N") (TName "integers")] env
            env <- sideCondition (SCEquality (TApp "is-less-or-equal" [TVar "M",TVar "N"]) (TName "false")) env
            rewriteTermTo (TSeq []) env

left_to_right_filter_ fargs = FApp "left-to-right-filter" (fargs)
stepLeft_to_right_filter fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "P",PAnnotated (PMetaVar "V") (TName "values"),PAnnotated (PSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TApp "left-to-right" [TApp "when-true" [TApp "give" [TVar "V",TVar "P"],TVar "V"],TApp "left-to-right-filter" [TVar "P",TVar "V*"]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- fsMatch fargs [PWildCard] env
            rewriteTermTo (TSeq []) env

interleave_filter_ fargs = FApp "interleave-filter" (fargs)
stepInterleave_filter fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "P",PAnnotated (PMetaVar "V") (TName "values"),PAnnotated (PSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TApp "interleave" [TApp "when-true" [TApp "give" [TVar "V",TVar "P"],TVar "V"],TApp "interleave-filter" [TVar "P",TVar "V*"]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- fsMatch fargs [PWildCard] env
            rewriteTermTo (TSeq []) env

fold_left_ fargs = FApp "fold-left" (fargs)
stepFold_left fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PWildCard,PAnnotated (PMetaVar "A") (TName "values")] env
            rewriteTermTo (TVar "A") env
          rewrite2 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "F",PAnnotated (PMetaVar "A") (TName "values"),PAnnotated (PMetaVar "V") (TName "values"),PAnnotated (PSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TApp "fold-left" [TVar "F",TApp "give" [TApp "tuple" [TVar "A",TVar "V"],TVar "F"],TVar "V*"]) env

fold_right_ fargs = FApp "fold-right" (fargs)
stepFold_right fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PWildCard,PAnnotated (PMetaVar "A") (TName "values")] env
            rewriteTermTo (TVar "A") env
          rewrite2 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "F",PAnnotated (PMetaVar "A") (TName "values"),PAnnotated (PSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp),PAnnotated (PMetaVar "V") (TName "values")] env
            rewriteTermTo (TApp "give" [TApp "tuple" [TVar "V",TApp "fold-right" [TVar "F",TVar "A",TVar "V*"]],TVar "F"]) env