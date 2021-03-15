-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Computations/Abnormal/Returning/Returning.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Abnormal.Returning.Returning where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("returning",DataTypeMemberss "returning" [] [DataTypeMemberConstructor "returned" [TName "values"] (Just [])])]

funcons = libFromList
    [("returned",StrictFuncon stepReturned),("finalise-returning",NonStrictFuncon stepFinalise_returning),("return",StrictFuncon stepReturn),("handle-return",NonStrictFuncon stepHandle_return),("returning",NullaryFuncon stepReturning)]

returned_ fargs = FApp "returned" (fargs)
stepReturned fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 117)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 100)])])),TVar "_X1"]) env

finalise_returning_ fargs = FApp "finalise-returning" (fargs)
stepFinalise_returning fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "X"] env
            rewriteTermTo (TApp "finalise-abrupting" [TVar "X"]) env

return_ fargs = FApp "return" (fargs)
stepReturn fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TApp "abrupt" [TApp "returned" [TVar "V"]]) env

handle_return_ fargs = FApp "handle-return" (fargs)
stepHandle_return fargs =
    evalRules [rewrite1] [step1,step2,step3]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "V") (TName "values")] env
            rewriteTermTo (TVar "V") env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getControlPatt "abrupted" (Nothing) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Nothing) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varabrupted (Nothing) env
            stepTermTo (TApp "handle-return" [TVar "X'"]) env
          step2 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getControlPatt "abrupted" (Nothing) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Just (TApp "returned" [TVar "V"])) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varabrupted (Just (PADT "returned" [VPAnnotated (VPMetaVar "V") (TName "values")])) env
            stepTermTo (TVar "V") env
          step3 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getControlPatt "abrupted" (Just (VPMetaVar "V'")) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Just (TVar "V'")) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varabrupted (Just (VPAnnotated (VPMetaVar "V'") (TSortComplement (TName "returning")))) env
            raiseTerm "abrupted" (TVar "V'") env
            stepTermTo (TApp "handle-return" [TVar "X'"]) env

returning_ = FName "returning"
stepReturning = rewriteType "returning" []