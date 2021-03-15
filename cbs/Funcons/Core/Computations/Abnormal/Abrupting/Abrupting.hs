-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Computations/Abnormal/Abrupting/Abrupting.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Abnormal.Abrupting.Abrupting where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("finalise-abrupting",NonStrictFuncon stepFinalise_abrupting),("abrupt",StrictFuncon stepAbrupt),("handle-abrupt",NonStrictFuncon stepHandle_abrupt),("finally",NonStrictFuncon stepFinally)]

finalise_abrupting_ fargs = FApp "finalise-abrupting" (fargs)
stepFinalise_abrupting fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "X"] env
            rewriteTermTo (TApp "handle-abrupt" [TVar "X",TName "null-value"]) env

abrupt_ fargs = FApp "abrupt" (fargs)
stepAbrupt fargs =
    evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values")] env
            env <- getControlPatt "abrupted" (Just (VPMetaVar "V")) env
            raiseTerm "abrupted" (TVar "V") env
            stepTermTo (TName "stuck") env

handle_abrupt_ fargs = FApp "handle-abrupt" (fargs)
stepHandle_abrupt fargs =
    evalRules [rewrite1] [step1,step2]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "V") (TName "values"),PMetaVar "Y"] env
            rewriteTermTo (TVar "V") env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X",PMetaVar "Y"] env
            env <- getControlPatt "abrupted" (Nothing) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Nothing) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varabrupted (Nothing) env
            stepTermTo (TApp "handle-abrupt" [TVar "X'",TVar "Y"]) env
          step2 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X",PMetaVar "Y"] env
            env <- getControlPatt "abrupted" (Nothing) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Just (TVar "V")) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varabrupted (Just (VPAnnotated (VPMetaVar "V") (TName "values"))) env
            stepTermTo (TApp "give" [TVar "V",TVar "Y"]) env

finally_ fargs = FApp "finally" (fargs)
stepFinally fargs =
    evalRules [rewrite1] [step1,step2]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "V") (TName "values"),PMetaVar "Y"] env
            rewriteTermTo (TApp "sequential" [TVar "Y",TVar "V"]) env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X",PMetaVar "Y"] env
            env <- getControlPatt "abrupted" (Nothing) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Nothing) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varabrupted (Nothing) env
            stepTermTo (TApp "finally" [TVar "X'",TVar "Y"]) env
          step2 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X",PMetaVar "Y"] env
            env <- getControlPatt "abrupted" (Nothing) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Just (TVar "V")) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varabrupted (Just (VPAnnotated (VPMetaVar "V") (TName "values"))) env
            stepTermTo (TApp "sequential" [TVar "Y",TApp "abrupt" [TVar "V"]]) env