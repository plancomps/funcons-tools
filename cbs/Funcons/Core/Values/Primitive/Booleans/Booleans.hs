-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Primitive/Booleans/Booleans.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Primitive.Booleans.Booleans where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("booleans",DataTypeMemberss "booleans" [] [DataTypeMemberConstructor "true" [] (Just []),DataTypeMemberConstructor "false" [] (Just [])]),("bools",DataTypeMemberss "bools" [] [DataTypeMemberConstructor "true" [] (Just []),DataTypeMemberConstructor "false" [] (Just [])])]

funcons = libFromList
    [("true",NullaryFuncon stepTrue),("false",NullaryFuncon stepFalse),("not",StrictFuncon stepNot),("implies",StrictFuncon stepImplies),("and",StrictFuncon stepAnd),("or",StrictFuncon stepOr),("exclusive-or",StrictFuncon stepExclusive_or),("xor",StrictFuncon stepExclusive_or),("booleans",NullaryFuncon stepBooleans),("bools",NullaryFuncon stepBooleans)]

true_ = FName "true"
stepTrue = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 117)]),FValue (ADTVal "unicode-character" [FValue (Int 101)])]))]) env

false_ = FName "false"
stepFalse = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 102)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 101)])]))]) env

not_ fargs = FApp "not" (fargs)
stepNot fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "false" []] env
            rewriteTermTo (TName "true") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "true" []] env
            rewriteTermTo (TName "false") env

implies_ fargs = FApp "implies" (fargs)
stepImplies fargs =
    evalRules [rewrite1,rewrite2,rewrite3,rewrite4] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "false" [],PADT "false" []] env
            rewriteTermTo (TName "true") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "false" [],PADT "true" []] env
            rewriteTermTo (TName "true") env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "true" [],PADT "true" []] env
            rewriteTermTo (TName "true") env
          rewrite4 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "true" [],PADT "false" []] env
            rewriteTermTo (TName "false") env

and_ fargs = FApp "and" (fargs)
stepAnd fargs =
    evalRules [rewrite1,rewrite2,rewrite3] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [] env
            rewriteTermTo (TName "true") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "false" [],VPAnnotated (VPSeqVar "___" StarOp) (TSortSeq (TName "booleans") StarOp)] env
            rewriteTermTo (TName "false") env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "true" [],VPAnnotated (VPSeqVar "B*" StarOp) (TSortSeq (TName "booleans") StarOp)] env
            rewriteTermTo (TApp "and" [TVar "B*"]) env

or_ fargs = FApp "or" (fargs)
stepOr fargs =
    evalRules [rewrite1,rewrite2,rewrite3] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [] env
            rewriteTermTo (TName "false") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "true" [],VPAnnotated (VPSeqVar "___" StarOp) (TSortSeq (TName "booleans") StarOp)] env
            rewriteTermTo (TName "true") env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "false" [],VPAnnotated (VPSeqVar "B*" StarOp) (TSortSeq (TName "booleans") StarOp)] env
            rewriteTermTo (TApp "or" [TVar "B*"]) env

exclusive_or_ fargs = FApp "exclusive-or" (fargs)
xor_ fargs = FApp "exclusive-or" (fargs)
stepExclusive_or fargs =
    evalRules [rewrite1,rewrite2,rewrite3,rewrite4] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "false" [],PADT "false" []] env
            rewriteTermTo (TName "false") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "false" [],PADT "true" []] env
            rewriteTermTo (TName "true") env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "true" [],PADT "false" []] env
            rewriteTermTo (TName "true") env
          rewrite4 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "true" [],PADT "true" []] env
            rewriteTermTo (TName "false") env

booleans_ = FName "booleans"
stepBooleans = rewriteType "booleans" []