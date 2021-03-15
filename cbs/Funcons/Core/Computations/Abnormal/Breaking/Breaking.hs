-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Computations/Abnormal/Breaking/Breaking.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Abnormal.Breaking.Breaking where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("breaking",DataTypeMemberss "breaking" [] [DataTypeMemberConstructor "broken" [] (Just [])])]

funcons = libFromList
    [("broken",NullaryFuncon stepBroken),("finalise-breaking",NonStrictFuncon stepFinalise_breaking),("break",NullaryFuncon stepBreak),("handle-break",NonStrictFuncon stepHandle_break),("breaking",NullaryFuncon stepBreaking)]

broken_ = FName "broken"
stepBroken = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 98)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 107)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 110)])]))]) env

finalise_breaking_ fargs = FApp "finalise-breaking" (fargs)
stepFinalise_breaking fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "X"] env
            rewriteTermTo (TApp "finalise-abrupting" [TVar "X"]) env

break_ = FName "break"
stepBreak = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "abrupt" [TName "broken"]) env

handle_break_ fargs = FApp "handle-break" (fargs)
stepHandle_break fargs =
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
            stepTermTo (TApp "handle-break" [TVar "X'"]) env
          step2 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getControlPatt "abrupted" (Nothing) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Just (TName "broken")) env (premise (TVar "X") [PWildCard] env))
            env <- receiveSignalPatt __varabrupted (Just (PADT "broken" [])) env
            stepTermTo (TName "null-value") env
          step3 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getControlPatt "abrupted" (Just (VPMetaVar "V")) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Just (TVar "V")) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varabrupted (Just (VPAnnotated (VPMetaVar "V") (TSortComplement (TName "breaking")))) env
            raiseTerm "abrupted" (TVar "V") env
            stepTermTo (TApp "handle-break" [TVar "X'"]) env

breaking_ = FName "breaking"
stepBreaking = rewriteType "breaking" []