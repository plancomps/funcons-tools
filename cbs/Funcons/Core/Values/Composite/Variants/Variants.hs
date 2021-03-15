-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Composite/Variants/Variants.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.Variants.Variants where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("variants",DataTypeMemberss "variants" [TPVar "T"] [DataTypeMemberConstructor "variant" [TName "identifiers",TVar "T"] (Just [TPVar "T"])])]

funcons = libFromList
    [("variant",StrictFuncon stepVariant),("variant-id",StrictFuncon stepVariant_id),("variant-value",StrictFuncon stepVariant_value),("variants",StrictFuncon stepVariants)]

variant_ fargs = FApp "variant" (fargs)
stepVariant fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1",VPMetaVar "_X2"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X2") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 118)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])])),TVar "_X1",TVar "_X2"]) env

variant_id_ fargs = FApp "variant-id" (fargs)
stepVariant_id fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "variant" [VPAnnotated (VPMetaVar "I") (TName "identifiers"),VPAnnotated VPWildCard (TName "values")]] env
            rewriteTermTo (TVar "I") env

variant_value_ fargs = FApp "variant-value" (fargs)
stepVariant_value fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "variant" [VPAnnotated VPWildCard (TName "identifiers"),VPAnnotated (VPMetaVar "V") (TName "values")]] env
            rewriteTermTo (TVar "V") env

variants_ = FApp "variants"
stepVariants ts = rewriteType "variants" ts