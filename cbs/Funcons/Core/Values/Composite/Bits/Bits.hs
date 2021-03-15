-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Composite/Bits/Bits.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.Bits.Bits where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("bit-vectors",DataTypeMemberss "bit-vectors" [TPVar "N"] [DataTypeMemberConstructor "bit-vector" [TSortPower (TName "bits") (TVar "N")] (Just [TPVar "N"])])]

funcons = libFromList
    [("bits",NullaryFuncon stepBits),("bit-vector",StrictFuncon stepBit_vector),("bytes",NullaryFuncon stepBytes),("octets",NullaryFuncon stepBytes),("unsigned-bit-vector-maximum",StrictFuncon stepUnsigned_bit_vector_maximum),("signed-bit-vector-maximum",StrictFuncon stepSigned_bit_vector_maximum),("signed-bit-vector-minimum",StrictFuncon stepSigned_bit_vector_minimum),("is-in-signed-bit-vector",StrictFuncon stepIs_in_signed_bit_vector),("is-in-unsigned-bit-vector",StrictFuncon stepIs_in_unsigned_bit_vector),("bit-vectors",StrictFuncon stepBit_vectors)]

bits_ = FName "bits"
stepBits = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TName "booleans") env

bit_vector_ fargs = FApp "bit-vector" (fargs)
stepBit_vector fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPSeqVar "_X1*" StarOp] env
            env <- sideCondition (SCIsInSort (TVar "_X1*") (TSortSeq (TSortSeq (TName "values") QuestionMarkOp) StarOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 98)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 45)]),FValue (ADTVal "unicode-character" [FValue (Int 118)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 114)])])),TVar "_X1*"]) env

bytes_ = FName "bytes"
octets_ = FName "bytes"
stepBytes = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "bit-vectors" [TFuncon (FValue (Nat 8))]) env

unsigned_bit_vector_maximum_ fargs = FApp "unsigned-bit-vector-maximum" (fargs)
stepUnsigned_bit_vector_maximum fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "N") (TName "natural-numbers")] env
            rewriteTermTo (TApp "integer-subtract" [TApp "integer-power" [TFuncon (FValue (Nat 2)),TVar "N"],TFuncon (FValue (Nat 1))]) env

signed_bit_vector_maximum_ fargs = FApp "signed-bit-vector-maximum" (fargs)
stepSigned_bit_vector_maximum fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "N") (TName "natural-numbers")] env
            rewriteTermTo (TApp "integer-subtract" [TApp "integer-power" [TFuncon (FValue (Nat 2)),TApp "integer-subtract" [TVar "N",TFuncon (FValue (Nat 1))]],TFuncon (FValue (Nat 1))]) env

signed_bit_vector_minimum_ fargs = FApp "signed-bit-vector-minimum" (fargs)
stepSigned_bit_vector_minimum fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "N") (TName "natural-numbers")] env
            rewriteTermTo (TApp "integer-negate" [TApp "integer-power" [TFuncon (FValue (Nat 2)),TApp "integer-subtract" [TVar "N",TFuncon (FValue (Nat 1))]]]) env

is_in_signed_bit_vector_ fargs = FApp "is-in-signed-bit-vector" (fargs)
stepIs_in_signed_bit_vector fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "M") (TName "integers"),VPAnnotated (VPMetaVar "N") (TName "natural-numbers")] env
            rewriteTermTo (TApp "and" [TApp "integer-is-less-or-equal" [TVar "M",TApp "signed-bit-vector-maximum" [TVar "N"]],TApp "integer-is-greater-or-equal" [TVar "M",TApp "signed-bit-vector-minimum" [TVar "N"]]]) env

is_in_unsigned_bit_vector_ fargs = FApp "is-in-unsigned-bit-vector" (fargs)
stepIs_in_unsigned_bit_vector fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "M") (TName "integers"),VPAnnotated (VPMetaVar "N") (TName "natural-numbers")] env
            rewriteTermTo (TApp "and" [TApp "integer-is-less-or-equal" [TVar "M",TApp "unsigned-bit-vector-maximum" [TVar "N"]],TApp "integer-is-greater-or-equal" [TVar "M",TFuncon (FValue (Nat 0))]]) env

bit_vectors_ = FApp "bit-vectors"
stepBit_vectors ts = rewriteType "bit-vectors" ts