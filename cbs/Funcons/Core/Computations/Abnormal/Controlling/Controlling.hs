-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Computations/Abnormal/Controlling/Controlling.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Abnormal.Controlling.Controlling where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("continuations",DataTypeMemberss "continuations" [TPVar "T1",TPVar "T2"] [DataTypeMemberConstructor "continuation" [TApp "abstractions" [TSortComputesFrom (TSeq []) (TVar "T2")]] (Just [TPVar "T1",TPVar "T2"])])]

funcons = libFromList
    [("continuation",StrictFuncon stepContinuation),("hole",NullaryFuncon stepHole),("resume-continuation",StrictFuncon stepResume_continuation),("control",StrictFuncon stepControl),("delimit-current-continuation",NonStrictFuncon stepDelimit_current_continuation),("delimit-cc",NonStrictFuncon stepDelimit_current_continuation),("continuations",StrictFuncon stepContinuations)]

continuation_ fargs = FApp "continuation" (fargs)
stepContinuation fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 117)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 110)])])),TVar "_X1"]) env

hole_ = FName "hole"
stepHole = evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- getControlPatt "plug-signal" (Just (VPMetaVar "V")) env
            raiseTerm "plug-signal" (TVar "V") env
            stepTermTo (TVar "V") env

resume_continuation_ fargs = FApp "resume-continuation" (fargs)
stepResume_continuation fargs =
    evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [PADT "continuation" [PADT "abstraction" [VPMetaVar "X"]],VPAnnotated (VPMetaVar "V") (TName "values")] env
            env <- getControlPatt "plug-signal" (Nothing) env
            (env,[__varplug_signal]) <- receiveSignals ["plug-signal"] (withControlTerm "plug-signal" (Just (TVar "V")) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varplug_signal (Just (VPMetaVar "V")) env
            stepTermTo (TVar "X'") env

control_ fargs = FApp "control" (fargs)
stepControl fargs =
    evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "F") (TApp "functions" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp])] env
            env <- getControlPatt "control-signal" (Just (VPMetaVar "F")) env
            raiseTerm "control-signal" (TVar "F") env
            stepTermTo (TName "hole") env

delimit_current_continuation_ fargs = FApp "delimit-current-continuation" (fargs)
delimit_cc_ fargs = FApp "delimit-current-continuation" (fargs)
stepDelimit_current_continuation fargs =
    evalRules [rewrite1] [step1,step2]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "V") (TName "values")] env
            rewriteTermTo (TVar "V") env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getControlPatt "control-signal" (Nothing) env
            (env,[__varcontrol_signal]) <- receiveSignals ["control-signal"] (withControlTerm "control-signal" (Nothing) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varcontrol_signal (Nothing) env
            stepTermTo (TApp "delimit-current-continuation" [TVar "X'"]) env
          step2 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getControlPatt "control-signal" (Nothing) env
            (env,[__varcontrol_signal]) <- receiveSignals ["control-signal"] (withControlTerm "control-signal" (Just (TVar "F")) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varcontrol_signal (Just (VPMetaVar "F")) env
            stepTermTo (TApp "delimit-current-continuation" [TApp "apply" [TVar "F",TApp "continuation" [TApp "closure" [TVar "X'"]]]]) env

continuations_ = FApp "continuations"
stepContinuations ts = rewriteType "continuations" ts