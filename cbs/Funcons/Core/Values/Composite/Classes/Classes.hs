-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Composite/Classes/Classes.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.Classes.Classes where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("classes",DataTypeMemberss "classes" [] [DataTypeMemberConstructor "class" [TApp "thunks" [TApp "references" [TName "objects"]],TName "environments",TSortSeq (TName "identifiers") StarOp] (Just [])])]

funcons = libFromList
    [("class",StrictFuncon stepClass),("class-instantiator",StrictFuncon stepClass_instantiator),("class-feature-map",StrictFuncon stepClass_feature_map),("class-superclass-name-sequence",StrictFuncon stepClass_superclass_name_sequence),("class-name-tree",StrictFuncon stepClass_name_tree),("is-subclass-name",StrictFuncon stepIs_subclass_name),("class-name-single-inheritance-feature-map",StrictFuncon stepClass_name_single_inheritance_feature_map),("classes",NullaryFuncon stepClasses)]

class_ fargs = FApp "class" (fargs)
stepClass fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1",VPMetaVar "_X2",VPSeqVar "_X3*" StarOp] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X2") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X3*") (TSortSeq (TSortSeq (TName "values") QuestionMarkOp) StarOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 115)])])),TVar "_X1",TVar "_X2",TVar "_X3*"]) env

class_instantiator_ fargs = FApp "class-instantiator" (fargs)
stepClass_instantiator fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "class" [VPAnnotated (VPMetaVar "Thunk") (TApp "thunks" [TSortSeq (TName "values") QuestionMarkOp]),VPAnnotated (VPMetaVar "Envs") (TName "environments"),VPAnnotated (VPSeqVar "C*" StarOp) (TSortSeq (TName "identifiers") StarOp)]] env
            rewriteTermTo (TVar "Thunk") env

class_feature_map_ fargs = FApp "class-feature-map" (fargs)
stepClass_feature_map fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "class" [VPAnnotated (VPMetaVar "Thunk") (TApp "thunks" [TSortSeq (TName "values") QuestionMarkOp]),VPAnnotated (VPMetaVar "Env") (TName "environments"),VPAnnotated (VPSeqVar "C*" StarOp) (TSortSeq (TName "identifiers") StarOp)]] env
            rewriteTermTo (TVar "Env") env

class_superclass_name_sequence_ fargs = FApp "class-superclass-name-sequence" (fargs)
stepClass_superclass_name_sequence fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "class" [VPAnnotated (VPMetaVar "Thunk") (TApp "thunks" [TSortSeq (TName "values") QuestionMarkOp]),VPAnnotated (VPMetaVar "Env") (TName "environments"),VPAnnotated (VPSeqVar "C*" StarOp) (TSortSeq (TName "identifiers") StarOp)]] env
            rewriteTermTo (TVar "C*") env

class_name_tree_ fargs = FApp "class-name-tree" (fargs)
stepClass_name_tree fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "C") (TName "identifiers")] env
            rewriteTermTo (TApp "tree" [TVar "C",TApp "interleave-map" [TApp "class-name-tree" [TName "given"],TApp "class-superclass-name-sequence" [TApp "bound-value" [TVar "C"]]]]) env

is_subclass_name_ fargs = FApp "is-subclass-name" (fargs)
stepIs_subclass_name fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "C") (TName "identifiers"),VPAnnotated (VPMetaVar "C'") (TName "identifiers")] env
            rewriteTermTo (TApp "is-in-set" [TVar "C",TSet [TApp "forest-value-sequence" [TApp "class-name-tree" [TVar "C'"]]]]) env

class_name_single_inheritance_feature_map_ fargs = FApp "class-name-single-inheritance-feature-map" (fargs)
stepClass_name_single_inheritance_feature_map fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "C") (TName "identifiers")] env
            rewriteTermTo (TApp "map-override" [TApp "interleave-map" [TApp "class-feature-map" [TApp "bound-value" [TName "given"]],TApp "single-branching-sequence" [TApp "class-name-tree" [TVar "C"]]]]) env

classes_ = FName "classes"
stepClasses = rewriteType "classes" []