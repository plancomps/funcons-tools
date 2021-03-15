-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Primitive/Null/Null.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Primitive.Null.Null where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("null-type",DataTypeMemberss "null-type" [] [DataTypeMemberConstructor "null-value" [] (Just [])])]

funcons = libFromList
    [("null-value",NullaryFuncon stepNull_value),("null",NullaryFuncon stepNull_value),("null-type",NullaryFuncon stepNull_type)]

null_value_ = FName "null-value"
null_ = FName "null-value"
stepNull_value = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 117)]),FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 45)]),FValue (ADTVal "unicode-character" [FValue (Int 118)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 117)]),FValue (ADTVal "unicode-character" [FValue (Int 101)])]))]) env

null_type_ = FName "null-type"
stepNull_type = rewriteType "null-type" []