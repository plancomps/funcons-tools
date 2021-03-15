-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Composite/ASTs/ASTs.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.ASTs.ASTs where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("asts",DataTypeMemberss "asts" [] [DataTypeMemberConstructor "ast-value" [TName "types",TName "values"] (Just []),DataTypeMemberConstructor "ast-term" [TName "tags",TSortSeq (TName "asts") StarOp] (Just [])])]

funcons = libFromList
    [("tags",NullaryFuncon stepTags),("ast-value",StrictFuncon stepAst_value),("ast-term",StrictFuncon stepAst_term),("asts",NullaryFuncon stepAsts)]

tags_ = FName "tags"
stepTags = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TName "strings") env

ast_value_ fargs = FApp "ast-value" (fargs)
stepAst_value fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1",VPMetaVar "_X2"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X2") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 45)]),FValue (ADTVal "unicode-character" [FValue (Int 118)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 117)]),FValue (ADTVal "unicode-character" [FValue (Int 101)])])),TVar "_X1",TVar "_X2"]) env

ast_term_ fargs = FApp "ast-term" (fargs)
stepAst_term fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1",VPSeqVar "_X2*" StarOp] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X2*") (TSortSeq (TSortSeq (TName "values") QuestionMarkOp) StarOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 45)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 109)])])),TVar "_X1",TVar "_X2*"]) env

asts_ = FName "asts"
stepAsts = rewriteType "asts" []