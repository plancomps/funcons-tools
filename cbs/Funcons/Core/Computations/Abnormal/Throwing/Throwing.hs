-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Computations/Abnormal/Throwing/Throwing.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Abnormal.Throwing.Throwing where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("throwing",DataTypeMemberss "throwing" [] [DataTypeMemberConstructor "thrown" [TName "values"] (Just [])])]

funcons = libFromList
    [("thrown",StrictFuncon stepThrown),("finalise-throwing",NonStrictFuncon stepFinalise_throwing),("throw",StrictFuncon stepThrow),("handle-thrown",NonStrictFuncon stepHandle_thrown),("handle-recursively",NonStrictFuncon stepHandle_recursively),("catch-else-throw",PartiallyStrictFuncon [Strict,NonStrict] NonStrict stepCatch_else_throw),("throwing",NullaryFuncon stepThrowing)]

thrown_ fargs = FApp "thrown" (fargs)
stepThrown fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 104)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 119)]),FValue (ADTVal "unicode-character" [FValue (Int 110)])])),TVar "_X1"]) env

finalise_throwing_ fargs = FApp "finalise-throwing" (fargs)
stepFinalise_throwing fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "X"] env
            rewriteTermTo (TApp "finalise-abrupting" [TVar "X"]) env

throw_ fargs = FApp "throw" (fargs)
stepThrow fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TApp "abrupt" [TApp "thrown" [TVar "V"]]) env

handle_thrown_ fargs = FApp "handle-thrown" (fargs)
stepHandle_thrown fargs =
    evalRules [rewrite1] [step1,step2,step3]
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
            stepTermTo (TApp "handle-thrown" [TVar "X'",TVar "Y"]) env
          step2 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X",PMetaVar "Y"] env
            env <- getControlPatt "abrupted" (Nothing) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Just (TApp "thrown" [TVar "V''"])) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varabrupted (Just (PADT "thrown" [VPAnnotated (VPMetaVar "V''") (TName "values")])) env
            stepTermTo (TApp "give" [TVar "V''",TVar "Y"]) env
          step3 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X",PMetaVar "Y"] env
            env <- getControlPatt "abrupted" (Just (VPMetaVar "V'")) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Just (TVar "V'")) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varabrupted (Just (VPAnnotated (VPMetaVar "V'") (TSortComplement (TName "throwing")))) env
            raiseTerm "abrupted" (TVar "V'") env
            stepTermTo (TApp "handle-thrown" [TVar "X'",TVar "Y"]) env

handle_recursively_ fargs = FApp "handle-recursively" (fargs)
stepHandle_recursively fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "X",PMetaVar "Y"] env
            rewriteTermTo (TApp "handle-thrown" [TVar "X",TApp "else" [TApp "handle-recursively" [TVar "Y",TVar "Y"],TApp "throw" [TName "given"]]]) env

catch_else_throw_ fargs = FApp "catch-else-throw" (fargs)
stepCatch_else_throw fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "P") (TName "values"),PMetaVar "Y"] env
            rewriteTermTo (TApp "else" [TApp "case-match" [TVar "P",TVar "Y"],TApp "throw" [TName "given"]]) env

throwing_ = FName "throwing"
stepThrowing = rewriteType "throwing" []