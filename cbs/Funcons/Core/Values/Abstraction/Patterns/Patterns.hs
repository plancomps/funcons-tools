-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Abstraction/Patterns/Patterns.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Abstraction.Patterns.Patterns where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("patterns",DataTypeMemberss "patterns" [] [DataTypeMemberConstructor "pattern" [TApp "abstractions" [TSortComputesFrom (TName "values") (TName "environments")]] (Just [])])]

funcons = libFromList
    [("pattern",StrictFuncon stepPattern),("pattern-any",NullaryFuncon stepPattern_any),("pattern-bind",StrictFuncon stepPattern_bind),("pattern-type",NonStrictFuncon stepPattern_type),("pattern-else",StrictFuncon stepPattern_else),("pattern-unite",StrictFuncon stepPattern_unite),("match",StrictFuncon stepMatch),("match-loosely",StrictFuncon stepMatch_loosely),("case-match",PartiallyStrictFuncon [Strict,NonStrict] NonStrict stepCase_match),("case-match-loosely",PartiallyStrictFuncon [Strict,NonStrict] NonStrict stepCase_match_loosely),("case-variant-value",StrictFuncon stepCase_variant_value),("patterns",NullaryFuncon stepPatterns)]

pattern_ fargs = FApp "pattern" (fargs)
stepPattern fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 112)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 110)])])),TVar "_X1"]) env

pattern_any_ = FName "pattern-any"
stepPattern_any = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "pattern" [TApp "abstraction" [TApp "map" []]]) env

pattern_bind_ fargs = FApp "pattern-bind" (fargs)
stepPattern_bind fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "I") (TName "identifiers")] env
            rewriteTermTo (TApp "pattern" [TApp "abstraction" [TApp "bind-value" [TVar "I",TName "given"]]]) env

pattern_type_ fargs = FApp "pattern-type" (fargs)
stepPattern_type fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "T"] env
            rewriteTermTo (TApp "pattern" [TApp "abstraction" [TApp "if-true-else" [TApp "is-in-type" [TName "given",TVar "T"],TApp "map" [],TName "fail"]]]) env

pattern_else_ fargs = FApp "pattern-else" (fargs)
stepPattern_else fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "P1") (TName "values"),VPAnnotated (VPMetaVar "P2") (TName "values")] env
            rewriteTermTo (TApp "pattern" [TApp "abstraction" [TApp "else" [TApp "match" [TName "given",TVar "P1"],TApp "match" [TName "given",TVar "P2"]]]]) env

pattern_unite_ fargs = FApp "pattern-unite" (fargs)
stepPattern_unite fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "P1") (TName "values"),VPAnnotated (VPMetaVar "P2") (TName "values")] env
            rewriteTermTo (TApp "pattern" [TApp "abstraction" [TApp "collateral" [TApp "match" [TName "given",TVar "P1"],TApp "match" [TName "given",TVar "P2"]]]]) env

match_ fargs = FApp "match" (fargs)
stepMatch fargs =
    evalRules [rewrite1,rewrite2,rewrite3,rewrite4,rewrite5] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values"),PADT "pattern" [PADT "abstraction" [VPMetaVar "X"]]] env
            rewriteTermTo (TApp "give" [TVar "V",TVar "X"]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPAnnotated (VPMetaVar "I1") (TName "identifiers"),VPAnnotated (VPSeqVar "V1*" StarOp) (TSortSeq (TName "values") StarOp)],PADT "datatype-value" [VPAnnotated (VPMetaVar "I2") (TName "identifiers"),VPAnnotated (VPSeqVar "V2*" StarOp) (TSortSeq (TName "values") StarOp)]] env
            env <- sideCondition (SCInequality (TVar "I2") (TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 112)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 110)])])))) env
            rewriteTermTo (TApp "sequential" [TApp "check-true" [TApp "is-equal" [TVar "I1",TVar "I2"]],TApp "check-true" [TApp "is-equal" [TApp "length" [TVar "V1*"],TApp "length" [TVar "V2*"]]],TApp "collateral" [TApp "interleave-map" [TApp "match" [TApp "tuple-elements" [TName "given"]],TApp "tuple-zip" [TApp "tuple" [TVar "V1*"],TApp "tuple" [TVar "V2*"]]]]]) env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "M1") (TApp "maps" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp]),VPAnnotated (VPMetaVar "M2") (TApp "maps" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp])] env
            env <- sideCondition (SCEquality (TApp "dom" [TVar "M2"]) (TSet [])) env
            rewriteTermTo (TApp "if-true-else" [TApp "is-equal" [TApp "dom" [TVar "M1"],TSet []],TApp "map" [],TName "fail"]) env
          rewrite4 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "M1") (TApp "maps" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp]),VPAnnotated (VPMetaVar "M2") (TApp "maps" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp])] env
            env <- sideCondition (SCInequality (TApp "dom" [TVar "M2"]) (TSet [])) env
            env <- sideCondition (SCPatternMatch (TApp "some-element" [TApp "dom" [TVar "M2"]]) [VPMetaVar "K"]) env
            rewriteTermTo (TApp "if-true-else" [TApp "is-in-set" [TVar "K",TApp "dom" [TVar "M1"]],TApp "collateral" [TApp "match" [TApp "map-lookup" [TVar "M1",TVar "K"],TApp "map-lookup" [TVar "M2",TVar "K"]],TApp "match" [TApp "map-delete" [TVar "M1",TSet [TVar "K"]],TApp "map-delete" [TVar "M2",TSet [TVar "K"]]]],TName "fail"]) env
          rewrite5 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values"),VPAnnotated (VPMetaVar "P") (TName "values")] env
            env <- sideCondition (SCIsInSort (TVar "P") (TSortComplement (TSortUnion (TName "datatype-values") (TApp "maps" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp])))) env
            rewriteTermTo (TApp "if-true-else" [TApp "is-equal" [TVar "V",TVar "P"],TApp "map" [],TName "fail"]) env

match_loosely_ fargs = FApp "match-loosely" (fargs)
stepMatch_loosely fargs =
    evalRules [rewrite1,rewrite2,rewrite3,rewrite4,rewrite5] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "values"),PADT "pattern" [PADT "abstraction" [VPMetaVar "X"]]] env
            rewriteTermTo (TApp "give" [TVar "V",TVar "X"]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPAnnotated (VPMetaVar "I1") (TName "identifiers"),VPAnnotated (VPSeqVar "V1*" StarOp) (TSortSeq (TName "values") StarOp)],PADT "datatype-value" [VPAnnotated (VPMetaVar "I2") (TName "identifiers"),VPAnnotated (VPSeqVar "V2*" StarOp) (TSortSeq (TName "values") StarOp)]] env
            env <- sideCondition (SCInequality (TVar "I2") (TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 112)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 110)])])))) env
            rewriteTermTo (TApp "sequential" [TApp "check-true" [TApp "is-equal" [TVar "I1",TVar "I2"]],TApp "check-true" [TApp "is-equal" [TApp "length" [TVar "V1*"],TApp "length" [TVar "V2*"]]],TApp "collateral" [TApp "interleave-map" [TApp "match-loosely" [TApp "tuple-elements" [TName "given"]],TApp "tuple-zip" [TApp "tuple" [TVar "V1*"],TApp "tuple" [TVar "V2*"]]]]]) env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "M1") (TApp "maps" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp]),VPAnnotated (VPMetaVar "M2") (TApp "maps" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp])] env
            env <- sideCondition (SCEquality (TApp "dom" [TVar "M2"]) (TSet [])) env
            rewriteTermTo (TApp "map" []) env
          rewrite4 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "M1") (TApp "maps" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp]),VPAnnotated (VPMetaVar "M2") (TApp "maps" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp])] env
            env <- sideCondition (SCInequality (TApp "dom" [TVar "M2"]) (TSet [])) env
            env <- sideCondition (SCPatternMatch (TApp "some-element" [TApp "dom" [TVar "M2"]]) [VPMetaVar "K"]) env
            rewriteTermTo (TApp "if-true-else" [TApp "is-in-set" [TVar "K",TApp "dom" [TVar "M1"]],TApp "collateral" [TApp "match-loosely" [TApp "map-lookup" [TVar "M1",TVar "K"],TApp "map-lookup" [TVar "M2",TVar "K"]],TApp "match-loosely" [TApp "map-delete" [TVar "M1",TSet [TVar "K"]],TApp "map-delete" [TVar "M2",TSet [TVar "K"]]]],TName "fail"]) env
          rewrite5 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "DV") (TName "values"),VPAnnotated (VPMetaVar "P") (TName "values")] env
            env <- sideCondition (SCIsInSort (TVar "P") (TSortComplement (TSortUnion (TName "datatype-values") (TApp "maps" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp])))) env
            rewriteTermTo (TApp "if-true-else" [TApp "is-equal" [TVar "DV",TVar "P"],TApp "map" [],TName "fail"]) env

case_match_ fargs = FApp "case-match" (fargs)
stepCase_match fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "P") (TName "values"),PMetaVar "X"] env
            rewriteTermTo (TApp "scope" [TApp "match" [TName "given",TVar "P"],TVar "X"]) env

case_match_loosely_ fargs = FApp "case-match-loosely" (fargs)
stepCase_match_loosely fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "P") (TName "values"),PMetaVar "X"] env
            rewriteTermTo (TApp "scope" [TApp "match-loosely" [TName "given",TVar "P"],TVar "X"]) env

case_variant_value_ fargs = FApp "case-variant-value" (fargs)
stepCase_variant_value fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "I") (TName "identifiers")] env
            rewriteTermTo (TApp "case-match" [TApp "variant" [TVar "I",TName "pattern-any"],TApp "variant-value" [TName "given"]]) env

patterns_ = FName "patterns"
stepPatterns = rewriteType "patterns" []