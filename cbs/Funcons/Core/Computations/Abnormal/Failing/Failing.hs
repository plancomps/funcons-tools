-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Computations/Abnormal/Failing/Failing.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Abnormal.Failing.Failing where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("failing",DataTypeMemberss "failing" [] [DataTypeMemberConstructor "failed" [] (Just [])])]

funcons = libFromList
    [("failed",NullaryFuncon stepFailed),("finalise-failing",NonStrictFuncon stepFinalise_failing),("fail",NullaryFuncon stepFail),("else",NonStrictFuncon stepElse),("else-choice",NonStrictFuncon stepElse_choice),("check-true",StrictFuncon stepCheck_true),("check",StrictFuncon stepCheck_true),("checked",StrictFuncon stepChecked),("failing",NullaryFuncon stepFailing)]

failed_ = FName "failed"
stepFailed = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 102)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 100)])]))]) env

finalise_failing_ fargs = FApp "finalise-failing" (fargs)
stepFinalise_failing fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "X"] env
            rewriteTermTo (TApp "finalise-abrupting" [TVar "X"]) env

fail_ = FName "fail"
stepFail = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "abrupt" [TName "failed"]) env

else_ fargs = FApp "else" (fargs)
stepElse fargs =
    evalRules [rewrite1,rewrite2] [step1,step2,step3]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "V") (TName "values"),PMetaVar "Y"] env
            rewriteTermTo (TVar "V") env
          rewrite2 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "X",PMetaVar "Y",PSeqVar "Z+" PlusOp] env
            rewriteTermTo (TApp "else" [TVar "X",TApp "else" [TVar "Y",TVar "Z+"]]) env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X",PMetaVar "Y"] env
            env <- getControlPatt "abrupted" (Nothing) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Nothing) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varabrupted (Nothing) env
            stepTermTo (TApp "else" [TVar "X'",TVar "Y"]) env
          step2 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X",PMetaVar "Y"] env
            env <- getControlPatt "abrupted" (Nothing) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Just (TName "failed")) env (premise (TVar "X") [PWildCard] env))
            env <- receiveSignalPatt __varabrupted (Just (PADT "failed" [])) env
            stepTermTo (TVar "Y") env
          step3 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X",PMetaVar "Y"] env
            env <- getControlPatt "abrupted" (Just (VPMetaVar "V")) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Just (TVar "V")) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varabrupted (Just (VPAnnotated (VPMetaVar "V") (TSortComplement (TName "failing")))) env
            raiseTerm "abrupted" (TVar "V") env
            stepTermTo (TApp "else" [TVar "X'",TVar "Y"]) env

else_choice_ fargs = FApp "else-choice" (fargs)
stepElse_choice fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PSeqVar "W*" StarOp,PMetaVar "X",PMetaVar "Y",PSeqVar "Z*" StarOp] env
            rewriteTermTo (TApp "choice" [TApp "else" [TVar "X",TApp "else-choice" [TVar "W*",TVar "Y",TVar "Z*"],TApp "else" [TVar "Y",TApp "else-choice" [TVar "W*",TVar "X",TVar "Z*"]]]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "X"] env
            rewriteTermTo (TVar "X") env

check_true_ fargs = FApp "check-true" (fargs)
check_ fargs = FApp "check-true" (fargs)
stepCheck_true fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "true" []] env
            rewriteTermTo (TName "null-value") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "false" []] env
            rewriteTermTo (TName "fail") env

checked_ fargs = FApp "checked" (fargs)
stepChecked fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TVar "V") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [] env
            rewriteTermTo (TName "fail") env

failing_ = FName "failing"
stepFailing = rewriteType "failing" []