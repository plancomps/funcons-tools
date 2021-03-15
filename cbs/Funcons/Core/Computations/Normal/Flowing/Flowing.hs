-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Computations/Normal/Flowing/Flowing.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Normal.Flowing.Flowing where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("yielding",DataTypeMemberss "yielding" [] [DataTypeMemberConstructor "signal" [] (Just [])])]

funcons = libFromList
    [("left-to-right",NonStrictFuncon stepLeft_to_right),("l-to-r",NonStrictFuncon stepLeft_to_right),("right-to-left",NonStrictFuncon stepRight_to_left),("r-to-l",NonStrictFuncon stepRight_to_left),("sequential",NonStrictFuncon stepSequential),("seq",NonStrictFuncon stepSequential),("effect",StrictFuncon stepEffect),("choice",NonStrictFuncon stepChoice),("if-true-else",PartiallyStrictFuncon [Strict,NonStrict,NonStrict] NonStrict stepIf_true_else),("if-else",PartiallyStrictFuncon [Strict,NonStrict,NonStrict] NonStrict stepIf_true_else),("while-true",NonStrictFuncon stepWhile_true),("while",NonStrictFuncon stepWhile_true),("do-while-true",NonStrictFuncon stepDo_while_true),("do-while",NonStrictFuncon stepDo_while_true),("interleave",StrictFuncon stepInterleave),("signal",NullaryFuncon stepSignal),("yield",NullaryFuncon stepYield),("yield-on-value",StrictFuncon stepYield_on_value),("yield-on-abrupt",NonStrictFuncon stepYield_on_abrupt),("yielding",NullaryFuncon stepYielding)]

left_to_right_ fargs = FApp "left-to-right" (fargs)
l_to_r_ fargs = FApp "left-to-right" (fargs)
stepLeft_to_right fargs =
    evalRules [rewrite1] [step1]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TVar "V*") env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PAnnotated (PSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp),PMetaVar "Y",PSeqVar "Z*" StarOp] env
            env <- premise (TVar "Y") [PMetaVar "Y'"] env
            stepTermTo (TApp "left-to-right" [TVar "V*",TVar "Y'",TVar "Z*"]) env

right_to_left_ fargs = FApp "right-to-left" (fargs)
r_to_l_ fargs = FApp "right-to-left" (fargs)
stepRight_to_left fargs =
    evalRules [rewrite1] [step1]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TVar "V*") env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PSeqVar "X*" StarOp,PMetaVar "Y",PAnnotated (PSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            env <- premise (TVar "Y") [PMetaVar "Y'"] env
            stepTermTo (TApp "right-to-left" [TVar "X*",TVar "Y'",TVar "V*"]) env

sequential_ fargs = FApp "sequential" (fargs)
seq_ fargs = FApp "sequential" (fargs)
stepSequential fargs =
    evalRules [rewrite1,rewrite2] [step1]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PValue (PADT "null-value" []),PSeqVar "Y+" PlusOp] env
            rewriteTermTo (TApp "sequential" [TVar "Y+"]) env
          rewrite2 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "Y"] env
            rewriteTermTo (TVar "Y") env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X",PSeqVar "Y+" PlusOp] env
            env <- premise (TVar "X") [PMetaVar "X'"] env
            stepTermTo (TApp "sequential" [TVar "X'",TVar "Y+"]) env

effect_ fargs = FApp "effect" (fargs)
stepEffect fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TName "null-value") env

choice_ fargs = FApp "choice" (fargs)
stepChoice fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PSeqVar "X*" StarOp,PMetaVar "Y",PSeqVar "Z*" StarOp] env
            rewriteTermTo (TVar "Y") env

if_true_else_ fargs = FApp "if-true-else" (fargs)
if_else_ fargs = FApp "if-true-else" (fargs)
stepIf_true_else fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PValue (PADT "true" []),PMetaVar "X",PMetaVar "Y"] env
            rewriteTermTo (TVar "X") env
          rewrite2 = do
            let env = emptyEnv
            env <- fsMatch fargs [PValue (PADT "false" []),PMetaVar "X",PMetaVar "Y"] env
            rewriteTermTo (TVar "Y") env

while_true_ fargs = FApp "while-true" (fargs)
while_ fargs = FApp "while-true" (fargs)
stepWhile_true fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "B",PMetaVar "X"] env
            rewriteTermTo (TApp "if-true-else" [TVar "B",TApp "sequential" [TVar "X",TApp "while-true" [TVar "B",TVar "X"]],TName "null-value"]) env

do_while_true_ fargs = FApp "do-while-true" (fargs)
do_while_ fargs = FApp "do-while-true" (fargs)
stepDo_while_true fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "X",PMetaVar "B"] env
            rewriteTermTo (TApp "sequential" [TVar "X",TApp "if-true-else" [TVar "B",TApp "do-while-true" [TVar "X",TVar "B"],TName "null-value"]]) env

interleave_ fargs = FApp "interleave" (fargs)
stepInterleave fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            rewriteTermTo (TVar "V*") env

signal_ = FName "signal"
stepSignal = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 103)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 108)])]))]) env

yield_ = FName "yield"
stepYield = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "yield-on-value" [TName "null-value"]) env

yield_on_value_ fargs = FApp "yield-on-value" (fargs)
stepYield_on_value fargs =
    evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values")] env
            env <- getControlPatt "yielded" (Just (PADT "signal" [])) env
            raiseTerm "yielded" (TName "signal") env
            stepTermTo (TVar "V") env

yield_on_abrupt_ fargs = FApp "yield-on-abrupt" (fargs)
stepYield_on_abrupt fargs =
    evalRules [rewrite1] [step1,step2]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "V") (TName "values")] env
            rewriteTermTo (TVar "V") env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getControlPatt "abrupt" (Just (VPMetaVar "V")) env
            env <- getControlPatt "yielded" (Just (PADT "signal" [])) env
            (env,[__varabrupt,__varyielded]) <- receiveSignals ["abrupt","yielded"] (withControlTerm "abrupt" (Just (TVar "V")) env (withControlTerm "yielded" (Just (TVar "___")) env (premise (TVar "X") [PMetaVar "X'"] env)))
            env <- receiveSignalPatt __varabrupt (Just (VPAnnotated (VPMetaVar "V") (TName "values"))) env
            env <- receiveSignalPatt __varyielded (Just (VPSeqVar "___" QuestionMarkOp)) env
            raiseTerm "abrupt" (TVar "V") env
            raiseTerm "yielded" (TName "signal") env
            stepTermTo (TApp "yield-on-abrupt" [TVar "X'"]) env
          step2 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getControlPatt "abrupt" (Nothing) env
            (env,[__varabrupt]) <- receiveSignals ["abrupt"] (withControlTerm "abrupt" (Nothing) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varabrupt (Nothing) env
            stepTermTo (TApp "yield-on-abrupt" [TVar "X'"]) env

yielding_ = FName "yielding"
stepYielding = rewriteType "yielding" []