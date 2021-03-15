-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Composite/Objects/Objects.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.Objects.Objects where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("objects",DataTypeMemberss "objects" [] [DataTypeMemberConstructor "object" [TName "atoms",TName "identifiers",TName "environments",TSortSeq (TName "objects") StarOp] (Just [])])]

funcons = libFromList
    [("object",StrictFuncon stepObject),("object-identity",StrictFuncon stepObject_identity),("object-class-name",StrictFuncon stepObject_class_name),("object-feature-map",StrictFuncon stepObject_feature_map),("object-subobject-sequence",StrictFuncon stepObject_subobject_sequence),("object-tree",StrictFuncon stepObject_tree),("object-single-inheritance-feature-map",StrictFuncon stepObject_single_inheritance_feature_map),("objects",NullaryFuncon stepObjects)]

object_ fargs = FApp "object" (fargs)
stepObject fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1",VPMetaVar "_X2",VPMetaVar "_X3",VPSeqVar "_X4*" StarOp] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X2") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X3") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X4*") (TSortSeq (TSortSeq (TName "values") QuestionMarkOp) StarOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 98)]),FValue (ADTVal "unicode-character" [FValue (Int 106)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])])),TVar "_X1",TVar "_X2",TVar "_X3",TVar "_X4*"]) env

object_identity_ fargs = FApp "object-identity" (fargs)
stepObject_identity fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "object" [VPAnnotated (VPMetaVar "A") (TName "atoms"),VPAnnotated VPWildCard (TName "identifiers"),VPAnnotated VPWildCard (TName "environments"),VPAnnotated (VPSeqVar "___" StarOp) (TSortSeq (TName "objects") StarOp)]] env
            rewriteTermTo (TVar "A") env

object_class_name_ fargs = FApp "object-class-name" (fargs)
stepObject_class_name fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "object" [VPAnnotated VPWildCard (TName "atoms"),VPAnnotated (VPMetaVar "C") (TName "identifiers"),VPAnnotated VPWildCard (TName "environments"),VPAnnotated (VPSeqVar "___" StarOp) (TSortSeq (TName "objects") StarOp)]] env
            rewriteTermTo (TVar "C") env

object_feature_map_ fargs = FApp "object-feature-map" (fargs)
stepObject_feature_map fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "object" [VPAnnotated VPWildCard (TName "atoms"),VPAnnotated VPWildCard (TName "identifiers"),VPAnnotated (VPMetaVar "Env") (TName "environments"),VPAnnotated (VPSeqVar "___" StarOp) (TSortSeq (TName "objects") StarOp)]] env
            rewriteTermTo (TVar "Env") env

object_subobject_sequence_ fargs = FApp "object-subobject-sequence" (fargs)
stepObject_subobject_sequence fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "object" [VPAnnotated VPWildCard (TName "atoms"),VPAnnotated VPWildCard (TName "identifiers"),VPAnnotated VPWildCard (TName "environments"),VPAnnotated (VPSeqVar "O*" StarOp) (TSortSeq (TName "objects") StarOp)]] env
            rewriteTermTo (TVar "O*") env

object_tree_ fargs = FApp "object-tree" (fargs)
stepObject_tree fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "O") (TName "objects")] env
            rewriteTermTo (TApp "tree" [TVar "O",TApp "interleave-map" [TApp "object-tree" [TName "given"],TApp "object-subobject-sequence" [TVar "O"]]]) env

object_single_inheritance_feature_map_ fargs = FApp "object-single-inheritance-feature-map" (fargs)
stepObject_single_inheritance_feature_map fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "O") (TName "objects")] env
            rewriteTermTo (TApp "map-override" [TApp "left-to-right-map" [TApp "object-feature-map" [TName "given"],TApp "single-branching-sequence" [TApp "object-tree" [TVar "O"]]]]) env

objects_ = FName "objects"
stepObjects = rewriteType "objects" []