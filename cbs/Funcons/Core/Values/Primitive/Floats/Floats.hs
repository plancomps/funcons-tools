-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Primitive/Floats/Floats.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Primitive.Floats.Floats where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("float-formats",DataTypeMemberss "float-formats" [] [DataTypeMemberConstructor "binary32" [] (Just []),DataTypeMemberConstructor "binary64" [] (Just []),DataTypeMemberConstructor "binary128" [] (Just []),DataTypeMemberConstructor "decimal64" [] (Just []),DataTypeMemberConstructor "decimal128" [] (Just [])])]

funcons = libFromList
    [("binary32",NullaryFuncon stepBinary32),("binary64",NullaryFuncon stepBinary64),("binary128",NullaryFuncon stepBinary128),("decimal64",NullaryFuncon stepDecimal64),("decimal128",NullaryFuncon stepDecimal128),("float-formats",NullaryFuncon stepFloat_formats)]

binary32_ = FName "binary32"
stepBinary32 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 98)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 121)]),FValue (ADTVal "unicode-character" [FValue (Int 51)]),FValue (ADTVal "unicode-character" [FValue (Int 50)])]))]) env

binary64_ = FName "binary64"
stepBinary64 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 98)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 121)]),FValue (ADTVal "unicode-character" [FValue (Int 54)]),FValue (ADTVal "unicode-character" [FValue (Int 52)])]))]) env

binary128_ = FName "binary128"
stepBinary128 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 98)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 121)]),FValue (ADTVal "unicode-character" [FValue (Int 49)]),FValue (ADTVal "unicode-character" [FValue (Int 50)]),FValue (ADTVal "unicode-character" [FValue (Int 56)])]))]) env

decimal64_ = FName "decimal64"
stepDecimal64 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 100)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 109)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 54)]),FValue (ADTVal "unicode-character" [FValue (Int 52)])]))]) env

decimal128_ = FName "decimal128"
stepDecimal128 = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 100)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 109)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 49)]),FValue (ADTVal "unicode-character" [FValue (Int 50)]),FValue (ADTVal "unicode-character" [FValue (Int 56)])]))]) env

float_formats_ = FName "float-formats"
stepFloat_formats = rewriteType "float-formats" []