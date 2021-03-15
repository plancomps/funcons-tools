-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Computations/Normal/Storing/Storing.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Normal.Storing.Storing where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("variables",DataTypeMemberss "variables" [] [DataTypeMemberConstructor "variable" [TName "locations",TName "value-types"] (Just [])]),("vars",DataTypeMemberss "vars" [] [DataTypeMemberConstructor "variable" [TName "locations",TName "value-types"] (Just [])])]

funcons = libFromList
    [("locations",NullaryFuncon stepLocations),("locs",NullaryFuncon stepLocations),("stores",NullaryFuncon stepStores),("store-clear",NullaryFuncon stepStore_clear),("initialise-storing",NonStrictFuncon stepInitialise_storing),("init-storing",NonStrictFuncon stepInitialise_storing),("variable",StrictFuncon stepVariable),("var",StrictFuncon stepVariable),("allocate-variable",StrictFuncon stepAllocate_variable),("alloc",StrictFuncon stepAllocate_variable),("recycle-variables",StrictFuncon stepRecycle_variables),("recycle",StrictFuncon stepRecycle_variables),("initialise-variable",StrictFuncon stepInitialise_variable),("init",StrictFuncon stepInitialise_variable),("allocate-initialised-variable",PartiallyStrictFuncon [NonStrict,Strict] NonStrict stepAllocate_initialised_variable),("alloc-init",PartiallyStrictFuncon [NonStrict,Strict] NonStrict stepAllocate_initialised_variable),("assign",StrictFuncon stepAssign),("assigned",StrictFuncon stepAssigned),("current-value",StrictFuncon stepCurrent_value),("un-assign",StrictFuncon stepUn_assign),("structural-assign",StrictFuncon stepStructural_assign),("structural-assigned",StrictFuncon stepStructural_assigned),("variables",NullaryFuncon stepVariables),("vars",NullaryFuncon stepVariables)]

locations_ = FName "locations"
locs_ = FName "locations"
stepLocations = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TName "atoms") env

stores_ = FName "stores"
stepStores = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "maps" [TName "locations",TSortSeq (TName "values") QuestionMarkOp]) env

store_clear_ = FName "store-clear"
stepStore_clear = evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- getMutPatt "store" (VPWildCard) env
            putMutTerm "store" (TApp "map" []) env
            stepTermTo (TName "null-value") env

initialise_storing_ fargs = FApp "initialise-storing" (fargs)
init_storing_ fargs = FApp "initialise-storing" (fargs)
stepInitialise_storing fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "X"] env
            rewriteTermTo (TApp "sequential" [TName "store-clear",TApp "initialise-giving" [TApp "initialise-generating" [TVar "X"]]]) env

variable_ fargs = FApp "variable" (fargs)
var_ fargs = FApp "variable" (fargs)
stepVariable fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1",VPMetaVar "_X2"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X2") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 118)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 98)]),FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 101)])])),TVar "_X1",TVar "_X2"]) env

allocate_variable_ fargs = FApp "allocate-variable" (fargs)
alloc_ fargs = FApp "allocate-variable" (fargs)
stepAllocate_variable fargs =
    evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "T") (TName "types")] env
            env <- getMutPatt "store" (VPMetaVar "Sigma") env
            putMutTerm "store" (TVar "Sigma") env
            env <- premise (TApp "use-atom-not-in" [TApp "dom" [TVar "Sigma"]]) [PMetaVar "L"] env
            env <- getMutPatt "store" (VPMetaVar "Sigma'") env
            env <- lifted_sideCondition (SCPatternMatch (TApp "map-override" [TMap [TBinding (TVar "L") (TSeq [])],TVar "Sigma'"]) [VPMetaVar "Sigma''"]) env
            putMutTerm "store" (TVar "Sigma''") env
            stepTermTo (TApp "variable" [TVar "L",TVar "T"]) env

recycle_variables_ fargs = FApp "recycle-variables" (fargs)
recycle_ fargs = FApp "recycle-variables" (fargs)
stepRecycle_variables fargs =
    evalRules [rewrite1] [step1,step2]
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "Var") (TName "variables"),VPAnnotated (VPSeqVar "Var+" PlusOp) (TSortSeq (TName "variables") PlusOp)] env
            rewriteTermTo (TApp "sequential" [TApp "recycle-variables" [TVar "Var"],TApp "recycle-variables" [TVar "Var+"]]) env
          step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [PADT "variable" [VPAnnotated (VPMetaVar "L") (TName "locations"),VPAnnotated (VPMetaVar "T") (TName "types")]] env
            env <- getMutPatt "store" (VPMetaVar "Sigma") env
            env <- lifted_sideCondition (SCEquality (TApp "is-in-set" [TVar "L",TApp "dom" [TVar "Sigma"]]) (TName "true")) env
            putMutTerm "store" (TApp "map-delete" [TVar "Sigma",TSet [TVar "L"]]) env
            stepTermTo (TName "null-value") env
          step2 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [PADT "variable" [VPAnnotated (VPMetaVar "L") (TName "locations"),VPAnnotated (VPMetaVar "T") (TName "types")]] env
            env <- getMutPatt "store" (VPMetaVar "Sigma") env
            env <- lifted_sideCondition (SCEquality (TApp "is-in-set" [TVar "L",TApp "dom" [TVar "Sigma"]]) (TName "false")) env
            putMutTerm "store" (TVar "Sigma") env
            stepTermTo (TName "fail") env

initialise_variable_ fargs = FApp "initialise-variable" (fargs)
init_ fargs = FApp "initialise-variable" (fargs)
stepInitialise_variable fargs =
    evalRules [] [step1,step2]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [PADT "variable" [VPAnnotated (VPMetaVar "L") (TName "locations"),VPAnnotated (VPMetaVar "T") (TName "types")],VPAnnotated (VPMetaVar "Val") (TName "values")] env
            env <- getMutPatt "store" (VPMetaVar "Sigma") env
            env <- lifted_sideCondition (SCEquality (TApp "and" [TApp "is-in-set" [TVar "L",TApp "dom" [TVar "Sigma"]],TApp "not" [TApp "is-value" [TApp "map-lookup" [TVar "Sigma",TVar "L"]]],TApp "is-in-type" [TVar "Val",TVar "T"]]) (TName "true")) env
            putMutTerm "store" (TApp "map-override" [TMap [TBinding (TVar "L") (TVar "Val")],TVar "Sigma"]) env
            stepTermTo (TName "null-value") env
          step2 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [PADT "variable" [VPAnnotated (VPMetaVar "L") (TName "locations"),VPAnnotated (VPMetaVar "T") (TName "types")],VPAnnotated (VPMetaVar "Val") (TName "values")] env
            env <- getMutPatt "store" (VPMetaVar "Sigma") env
            env <- lifted_sideCondition (SCEquality (TApp "and" [TApp "is-in-set" [TVar "L",TApp "dom" [TVar "Sigma"]],TApp "not" [TApp "is-value" [TApp "map-lookup" [TVar "Sigma",TVar "L"]]],TApp "is-in-type" [TVar "Val",TVar "T"]]) (TName "false")) env
            putMutTerm "store" (TVar "Sigma") env
            stepTermTo (TName "fail") env

allocate_initialised_variable_ fargs = FApp "allocate-initialised-variable" (fargs)
alloc_init_ fargs = FApp "allocate-initialised-variable" (fargs)
stepAllocate_initialised_variable fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "T",PAnnotated (PMetaVar "Val") (TVar "T")] env
            rewriteTermTo (TApp "give" [TApp "allocate-variable" [TVar "T"],TApp "sequential" [TApp "initialise-variable" [TName "given",TVar "Val"],TName "given"]]) env

assign_ fargs = FApp "assign" (fargs)
stepAssign fargs =
    evalRules [] [step1,step2]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [PADT "variable" [VPAnnotated (VPMetaVar "L") (TName "locations"),VPAnnotated (VPMetaVar "T") (TName "types")],VPAnnotated (VPMetaVar "Val") (TName "values")] env
            env <- getMutPatt "store" (VPMetaVar "Sigma") env
            env <- lifted_sideCondition (SCEquality (TApp "and" [TApp "is-in-set" [TVar "L",TApp "dom" [TVar "Sigma"]],TApp "is-in-type" [TVar "Val",TVar "T"]]) (TName "true")) env
            putMutTerm "store" (TApp "map-override" [TMap [TBinding (TVar "L") (TVar "Val")],TVar "Sigma"]) env
            stepTermTo (TName "null-value") env
          step2 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [PADT "variable" [VPAnnotated (VPMetaVar "L") (TName "locations"),VPAnnotated (VPMetaVar "T") (TName "types")],VPAnnotated (VPMetaVar "Val") (TName "values")] env
            env <- getMutPatt "store" (VPMetaVar "Sigma") env
            env <- lifted_sideCondition (SCEquality (TApp "and" [TApp "is-in-set" [TVar "L",TApp "dom" [TVar "Sigma"]],TApp "is-in-type" [TVar "Val",TVar "T"]]) (TName "false")) env
            putMutTerm "store" (TVar "Sigma") env
            stepTermTo (TName "fail") env

assigned_ fargs = FApp "assigned" (fargs)
stepAssigned fargs =
    evalRules [] [step1,step2]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [PADT "variable" [VPAnnotated (VPMetaVar "L") (TName "locations"),VPAnnotated (VPMetaVar "T") (TName "types")]] env
            env <- getMutPatt "store" (VPMetaVar "Sigma") env
            env <- lifted_sideCondition (SCPatternMatch (TApp "map-lookup" [TVar "Sigma",TVar "L"]) [VPAnnotated (VPMetaVar "Val") (TName "values")]) env
            putMutTerm "store" (TVar "Sigma") env
            stepTermTo (TVar "Val") env
          step2 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [PADT "variable" [VPAnnotated (VPMetaVar "L") (TName "locations"),VPAnnotated (VPMetaVar "T") (TName "types")]] env
            env <- getMutPatt "store" (VPMetaVar "Sigma") env
            env <- lifted_sideCondition (SCEquality (TApp "map-lookup" [TVar "Sigma",TVar "L"]) (TSeq [])) env
            putMutTerm "store" (TVar "Sigma") env
            stepTermTo (TName "fail") env

current_value_ fargs = FApp "current-value" (fargs)
stepCurrent_value fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "Var") (TName "variables")] env
            rewriteTermTo (TApp "assigned" [TVar "Var"]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "U") (TSortComplement (TName "variables"))] env
            rewriteTermTo (TVar "U") env

un_assign_ fargs = FApp "un-assign" (fargs)
stepUn_assign fargs =
    evalRules [] [step1,step2]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [PADT "variable" [VPAnnotated (VPMetaVar "L") (TName "locations"),VPAnnotated (VPMetaVar "T") (TName "types")]] env
            env <- getMutPatt "store" (VPMetaVar "Sigma") env
            env <- lifted_sideCondition (SCEquality (TApp "is-in-set" [TVar "L",TApp "dom" [TVar "Sigma"]]) (TName "true")) env
            putMutTerm "store" (TApp "map-override" [TMap [TBinding (TVar "L") (TSeq [])],TVar "Sigma"]) env
            stepTermTo (TName "null-value") env
          step2 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [PADT "variable" [VPAnnotated (VPMetaVar "L") (TName "locations"),VPAnnotated (VPMetaVar "T") (TName "types")]] env
            env <- getMutPatt "store" (VPMetaVar "Sigma") env
            env <- lifted_sideCondition (SCEquality (TApp "is-in-set" [TVar "L",TApp "dom" [TVar "Sigma"]]) (TName "false")) env
            putMutTerm "store" (TVar "Sigma") env
            stepTermTo (TName "fail") env

structural_assign_ fargs = FApp "structural-assign" (fargs)
stepStructural_assign fargs =
    evalRules [rewrite1,rewrite2,rewrite3,rewrite4,rewrite5] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V1") (TName "variables"),VPAnnotated (VPMetaVar "V2") (TName "values")] env
            rewriteTermTo (TApp "assign" [TVar "V1",TVar "V2"]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V1") (TName "datatype-values"),VPAnnotated (VPMetaVar "V2") (TName "datatype-values")] env
            env <- sideCondition (SCIsInSort (TVar "V1") (TSortComplement (TName "variables"))) env
            env <- sideCondition (SCPatternMatch (TVar "V1") [PADT "datatype-value" [VPAnnotated (VPMetaVar "I1") (TName "identifiers"),VPAnnotated (VPSeqVar "V1*" StarOp) (TSortSeq (TName "values") StarOp)]]) env
            env <- sideCondition (SCPatternMatch (TVar "V2") [PADT "datatype-value" [VPAnnotated (VPMetaVar "I2") (TName "identifiers"),VPAnnotated (VPSeqVar "V2*" StarOp) (TSortSeq (TName "values") StarOp)]]) env
            rewriteTermTo (TApp "sequential" [TApp "check-true" [TApp "is-equal" [TVar "I1",TVar "I2"]],TApp "effect" [TApp "tuple" [TApp "interleave-map" [TApp "structural-assign" [TApp "tuple-elements" [TName "given"]],TApp "tuple-zip" [TApp "tuple" [TVar "V1*"],TApp "tuple" [TVar "V2*"]]]]],TName "null-value"]) env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "M1") (TApp "maps" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp]),VPAnnotated (VPMetaVar "M2") (TApp "maps" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp])] env
            env <- sideCondition (SCEquality (TApp "dom" [TVar "M1"]) (TSet [])) env
            rewriteTermTo (TApp "check-true" [TApp "is-equal" [TApp "dom" [TVar "M2"],TSet []]]) env
          rewrite4 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "M1") (TApp "maps" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp]),VPAnnotated (VPMetaVar "M2") (TApp "maps" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp])] env
            env <- sideCondition (SCPatternMatch (TApp "some-element" [TApp "dom" [TVar "M1"]]) [VPMetaVar "K"]) env
            rewriteTermTo (TApp "sequential" [TApp "check-true" [TApp "is-in-set" [TVar "K",TApp "dom" [TVar "M2"]]],TApp "structural-assign" [TApp "map-lookup" [TVar "M1",TVar "K"],TApp "map-lookup" [TVar "M2",TVar "K"]],TApp "structural-assign" [TApp "map-delete" [TVar "M1",TSet [TVar "K"]],TApp "map-delete" [TVar "M2",TSet [TVar "K"]]]]) env
          rewrite5 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V1") (TName "values"),VPAnnotated (VPMetaVar "V2") (TName "values")] env
            env <- sideCondition (SCIsInSort (TVar "V1") (TSortComplement (TSortUnion (TName "datatype-values") (TApp "maps" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp])))) env
            rewriteTermTo (TApp "check-true" [TApp "is-equal" [TVar "V1",TVar "V2"]]) env

structural_assigned_ fargs = FApp "structural-assigned" (fargs)
stepStructural_assigned fargs =
    evalRules [rewrite1,rewrite2,rewrite3,rewrite4] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "Var") (TName "variables")] env
            rewriteTermTo (TApp "assigned" [TVar "Var"]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TName "datatype-values")] env
            env <- sideCondition (SCIsInSort (TVar "V") (TSortComplement (TName "variables"))) env
            env <- sideCondition (SCPatternMatch (TVar "V") [PADT "datatype-value" [VPAnnotated (VPMetaVar "I") (TName "identifiers"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)]]) env
            rewriteTermTo (TApp "datatype-value" [TVar "I",TApp "interleave-map" [TApp "structural-assigned" [TName "given"],TVar "V*"]]) env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "M") (TApp "maps" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp])] env
            rewriteTermTo (TApp "map" [TApp "interleave-map" [TApp "structural-assigned" [TName "given"],TApp "map-elements" [TVar "M"]]]) env
          rewrite4 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "U") (TName "values")] env
            env <- sideCondition (SCIsInSort (TVar "U") (TSortComplement (TSortUnion (TName "datatype-values") (TApp "maps" [TSortSeq (TName "values") QuestionMarkOp,TSortSeq (TName "values") QuestionMarkOp])))) env
            rewriteTermTo (TVar "U") env

variables_ = FName "variables"
stepVariables = rewriteType "variables" []