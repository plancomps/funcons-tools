-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Computations/Normal/Interacting/Interacting.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Normal.Interacting.Interacting where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("print",StrictFuncon stepPrint),("read",NullaryFuncon stepRead)]

print_ fargs = FApp "print" (fargs)
stepPrint fargs =
    evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)] env
            writeOutTerm "standard-out" (TVar "V*") env
            stepTermTo (TName "null-value") env

read_ = FName "read"
stepRead = evalRules [] [step1,step2]
    where step1 = do
            let env = emptyEnv
            env <- matchInput "standard-in" (VPAnnotated (VPMetaVar "V") (TSortComplement (TName "null-type"))) env
            stepTermTo (TVar "V") env
          step2 = do
            let env = emptyEnv
            env <- matchInput "standard-in" (PADT "null-value" []) env
            stepTermTo (TName "fail") env