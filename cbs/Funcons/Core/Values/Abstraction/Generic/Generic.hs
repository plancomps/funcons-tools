-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Abstraction/Generic/Generic.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Abstraction.Generic.Generic where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("abstractions",DataTypeMemberss "abstractions" [TPWildCard] [DataTypeMemberConstructor "abstraction" [TSortComputesFrom (TVar "T?") (TVar "T")] (Just [TPComputesFrom (TPVar "T?") (TPVar "T")])])]

funcons = libFromList
    [("abstraction",NonStrictFuncon stepAbstraction),("closure",NonStrictFuncon stepClosure),("enact",StrictFuncon stepEnact),("abstractions",StrictFuncon stepAbstractions)]

abstraction_ fargs = FApp "abstraction" (fargs)
stepAbstraction fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "_X1"] env
            rewriteTermTo (TApp "non-strict-datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 98)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 110)])])),TVar "_X1"]) env

closure_ fargs = FApp "closure" (fargs)
stepClosure fargs =
    evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getInhPatt "environment" [VPMetaVar "Rho"] env
            stepTermTo (TApp "abstraction" [TApp "closed" [TApp "scope" [TVar "Rho",TVar "X"]]]) env

enact_ fargs = FApp "enact" (fargs)
stepEnact fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "abstraction" [VPMetaVar "X"]] env
            rewriteTermTo (TVar "X") env

abstractions_ = FApp "abstractions"
stepAbstractions ts = rewriteType "abstractions" ts