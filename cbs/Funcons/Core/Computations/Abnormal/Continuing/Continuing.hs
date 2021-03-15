-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Computations/Abnormal/Continuing/Continuing.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Abnormal.Continuing.Continuing where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("continuing",DataTypeMemberss "continuing" [] [DataTypeMemberConstructor "continued" [] (Just [])])]

funcons = libFromList
    [("continued",NullaryFuncon stepContinued),("finalise-continuing",NonStrictFuncon stepFinalise_continuing),("continue",NullaryFuncon stepContinue),("handle-continue",NonStrictFuncon stepHandle_continue),("continuing",NullaryFuncon stepContinuing)]

continued_ = FName "continued"
stepContinued = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 117)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 100)])]))]) env

finalise_continuing_ fargs = FApp "finalise-continuing" (fargs)
stepFinalise_continuing fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "X"] env
            rewriteTermTo (TApp "finalise-abrupting" [TVar "X"]) env

continue_ = FName "continue"
stepContinue = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "abrupt" [TName "continued"]) env

handle_continue_ fargs = FApp "handle-continue" (fargs)
stepHandle_continue fargs =
    evalRules [rewrite1] [step1,step2,step3]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PValue (PADT "null-value" [])] env
            rewriteTermTo (TName "null-value") env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getControlPatt "abrupted" (Nothing) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Nothing) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varabrupted (Nothing) env
            stepTermTo (TApp "handle-continue" [TVar "X'"]) env
          step2 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getControlPatt "abrupted" (Nothing) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Just (TName "continued")) env (premise (TVar "X") [PWildCard] env))
            env <- receiveSignalPatt __varabrupted (Just (PADT "continued" [])) env
            stepTermTo (TName "null-value") env
          step3 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getControlPatt "abrupted" (Just (VPMetaVar "V")) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Just (TVar "V")) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varabrupted (Just (VPAnnotated (VPMetaVar "V") (TSortComplement (TName "continuing")))) env
            raiseTerm "abrupted" (TVar "V") env
            stepTermTo (TApp "handle-continue" [TVar "X'"]) env

continuing_ = FName "continuing"
stepContinuing = rewriteType "continuing" []