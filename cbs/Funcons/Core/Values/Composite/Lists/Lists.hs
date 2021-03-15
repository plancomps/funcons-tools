-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Composite/Lists/Lists.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.Lists.Lists where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("lists",DataTypeMemberss "lists" [TPVar "T"] [DataTypeMemberConstructor "list" [TSortSeq (TVar "T") StarOp] (Just [TPVar "T"])])]

funcons = libFromList
    [("list",StrictFuncon stepList),("list-elements",StrictFuncon stepList_elements),("list-nil",NullaryFuncon stepList_nil),("nil",NullaryFuncon stepList_nil),("list-cons",StrictFuncon stepList_cons),("cons",StrictFuncon stepList_cons),("list-head",StrictFuncon stepList_head),("head",StrictFuncon stepList_head),("list-tail",StrictFuncon stepList_tail),("tail",StrictFuncon stepList_tail),("list-length",StrictFuncon stepList_length),("list-append",StrictFuncon stepList_append),("lists",StrictFuncon stepLists)]

list_ fargs = FApp "list" (fargs)
stepList fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPSeqVar "_X1*" StarOp] env
            env <- sideCondition (SCIsInSort (TVar "_X1*") (TSortSeq (TSortSeq (TName "values") QuestionMarkOp) StarOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])])),TVar "_X1*"]) env

list_elements_ fargs = FApp "list-elements" (fargs)
stepList_elements fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "list" [VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)]] env
            rewriteTermTo (TVar "V*") env

list_nil_ = FName "list-nil"
nil_ = FName "list-nil"
stepList_nil = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "list" []) env

list_cons_ fargs = FApp "list-cons" (fargs)
cons_ fargs = FApp "list-cons" (fargs)
stepList_cons fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values"),PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)]] env
            rewriteTermTo (TApp "list" [TVar "V",TVar "V*"]) env

list_head_ fargs = FApp "list-head" (fargs)
head_ fargs = FApp "list-head" (fargs)
stepList_head fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPSeqVar "___" StarOp) (TSortSeq (TName "values") StarOp)]] env
            rewriteTermTo (TVar "V") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])])]] env
            rewriteTermTo (TSeq []) env

list_tail_ fargs = FApp "list-tail" (fargs)
tail_ fargs = FApp "list-tail" (fargs)
stepList_tail fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),VPAnnotated VPWildCard (TName "values"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)]] env
            rewriteTermTo (TApp "list" [TVar "V*"]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])])]] env
            rewriteTermTo (TSeq []) env

list_length_ fargs = FApp "list-length" (fargs)
stepList_length fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)]] env
            rewriteTermTo (TApp "length" [TVar "V*"]) env

list_append_ fargs = FApp "list-append" (fargs)
stepList_append fargs =
    evalRules [rewrite1,rewrite2,rewrite3,rewrite4] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),VPAnnotated (VPSeqVar "V1*" StarOp) (TSortSeq (TName "values") StarOp)],PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),VPAnnotated (VPSeqVar "V2*" StarOp) (TSortSeq (TName "values") StarOp)]] env
            rewriteTermTo (TApp "list" [TVar "V1*",TVar "V2*"]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "L1") (TApp "lists" [TSortSeq (TName "values") QuestionMarkOp]),VPAnnotated (VPMetaVar "L2") (TApp "lists" [TSortSeq (TName "values") QuestionMarkOp]),VPAnnotated (VPMetaVar "L3") (TApp "lists" [TSortSeq (TName "values") QuestionMarkOp]),VPAnnotated (VPSeqVar "L*" StarOp) (TSortSeq (TApp "lists" [TSortSeq (TName "values") QuestionMarkOp]) StarOp)] env
            rewriteTermTo (TApp "list-append" [TVar "L1",TApp "list-append" [TVar "L2",TVar "L3",TVar "L*"]]) env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [] env
            rewriteTermTo (TApp "list" []) env
          rewrite4 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "L") (TApp "lists" [TSortSeq (TName "values") QuestionMarkOp])] env
            rewriteTermTo (TVar "L") env

lists_ = FApp "lists"
stepLists ts = rewriteType "lists" ts