-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Composite/Tuples/Tuples.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.Tuples.Tuples where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("tuples",DataTypeMemberss "tuples" [TPSeqVar "T*" StarOp] [DataTypeMemberConstructor "tuple" [TVar "T*"] (Just [TPSeqVar "T*" StarOp])])]

funcons = libFromList
    [("tuple",StrictFuncon stepTuple),("tuple-elements",StrictFuncon stepTuple_elements),("tuple-zip",StrictFuncon stepTuple_zip),("tuples",StrictFuncon stepTuples)]

tuple_ fargs = FApp "tuple" (fargs)
stepTuple fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPSeqVar "_X1*" StarOp] env
            env <- sideCondition (SCIsInSort (TVar "_X1*") (TSortSeq (TSortSeq (TName "values") QuestionMarkOp) StarOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 117)]),FValue (ADTVal "unicode-character" [FValue (Int 112)]),FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 101)])])),TVar "_X1*"]) env

tuple_elements_ fargs = FApp "tuple-elements" (fargs)
stepTuple_elements fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "tuple" [VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)]] env
            rewriteTermTo (TVar "V*") env

tuple_zip_ fargs = FApp "tuple-zip" (fargs)
stepTuple_zip fargs =
    evalRules [rewrite1,rewrite2,rewrite3,rewrite4] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "tuple" [VPAnnotated (VPMetaVar "V1") (TName "values"),VPAnnotated (VPSeqVar "V1*" StarOp) (TSortSeq (TName "values") StarOp)],PADT "tuple" [VPAnnotated (VPMetaVar "V2") (TName "values"),VPAnnotated (VPSeqVar "V2*" StarOp) (TSortSeq (TName "values") StarOp)]] env
            rewriteTermTo (TSeq [TApp "tuple" [TVar "V1",TVar "V2"],TApp "tuple-zip" [TApp "tuple" [TVar "V1*"],TApp "tuple" [TVar "V2*"]]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "tuple" [],PADT "tuple" []] env
            rewriteTermTo (TSeq []) env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "tuple" [VPAnnotated (VPSeqVar "V1+" PlusOp) (TSortSeq (TName "values") PlusOp)],PADT "tuple" []] env
            rewriteTermTo (TSeq []) env
          rewrite4 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "tuple" [],PADT "tuple" [VPAnnotated (VPSeqVar "V2+" PlusOp) (TSortSeq (TName "values") PlusOp)]] env
            rewriteTermTo (TSeq []) env

tuples_ = FApp "tuples"
stepTuples ts = rewriteType "tuples" ts