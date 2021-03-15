-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Composite/References/References.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.References.References where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("references",DataTypeMemberss "references" [TPVar "T"] [DataTypeMemberConstructor "reference" [TVar "T"] (Just [TPVar "T"])]),("pointers",DataTypeMemberss "pointers" [TPVar "T"] [DataTypeInclusionn (TApp "references" [TVar "T"]),DataTypeMemberConstructor "pointer-null" [] (Just [TPVar "T"])])]

funcons = libFromList
    [("reference",StrictFuncon stepReference),("pointer-null",NullaryFuncon stepPointer_null),("dereference",StrictFuncon stepDereference),("references",StrictFuncon stepReferences),("pointers",StrictFuncon stepPointers)]

reference_ fargs = FApp "reference" (fargs)
stepReference fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 102)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 101)])])),TVar "_X1"]) env

pointer_null_ = FName "pointer-null"
stepPointer_null = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 112)]),FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 45)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 117)]),FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 108)])]))]) env

dereference_ fargs = FApp "dereference" (fargs)
stepDereference fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "reference" [VPAnnotated (VPMetaVar "V") (TName "values")]] env
            rewriteTermTo (TVar "V") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "pointer-null" []] env
            rewriteTermTo (TSeq []) env

references_ = FApp "references"
stepReferences ts = rewriteType "references" ts

pointers_ = FApp "pointers"
stepPointers ts = rewriteType "pointers" ts