-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Composite/Records/Records.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.Records.Records where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("records",DataTypeMemberss "records" [TPVar "T"] [DataTypeMemberConstructor "record" [TApp "maps" [TName "identifiers",TVar "T"]] (Just [TPVar "T"])])]

funcons = libFromList
    [("record",StrictFuncon stepRecord),("record-map",StrictFuncon stepRecord_map),("record-select",StrictFuncon stepRecord_select),("records",StrictFuncon stepRecords)]

record_ fargs = FApp "record" (fargs)
stepRecord fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 100)])])),TVar "_X1"]) env

record_map_ fargs = FApp "record-map" (fargs)
stepRecord_map fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "record" [VPAnnotated (VPMetaVar "M") (TApp "maps" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp])]] env
            rewriteTermTo (TVar "M") env

record_select_ fargs = FApp "record-select" (fargs)
stepRecord_select fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "R") (TApp "records" [TName "values"]),VPAnnotated (VPMetaVar "I") (TName "identifiers")] env
            rewriteTermTo (TApp "map-lookup" [TApp "record-map" [TVar "R"],TVar "I"]) env

records_ = FApp "records"
stepRecords ts = rewriteType "records" ts