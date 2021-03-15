-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Composite/Vectors/Vectors.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.Vectors.Vectors where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("vectors",DataTypeMemberss "vectors" [TPVar "T"] [DataTypeMemberConstructor "vector" [TSortSeq (TVar "T") StarOp] (Just [TPVar "T"])])]

funcons = libFromList
    [("vector",StrictFuncon stepVector),("vector-elements",StrictFuncon stepVector_elements),("vectors",StrictFuncon stepVectors)]

vector_ fargs = FApp "vector" (fargs)
stepVector fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPSeqVar "_X1*" StarOp] env
            env <- sideCondition (SCIsInSort (TVar "_X1*") (TSortSeq (TSortSeq (TName "values") QuestionMarkOp) StarOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 118)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 114)])])),TVar "_X1*"]) env

vector_elements_ fargs = FApp "vector-elements" (fargs)
stepVector_elements fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "vector" [VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)]] env
            rewriteTermTo (TVar "V*") env

vectors_ = FApp "vectors"
stepVectors ts = rewriteType "vectors" ts