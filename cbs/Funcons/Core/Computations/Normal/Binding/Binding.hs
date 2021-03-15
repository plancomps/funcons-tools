-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Computations/Normal/Binding/Binding.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Normal.Binding.Binding where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("identifiers",DataTypeMemberss "identifiers" [] [DataTypeInclusionn (TName "strings"),DataTypeMemberConstructor "identifier-tagged" [TName "identifiers",TName "values"] (Just [])]),("ids",DataTypeMemberss "ids" [] [DataTypeInclusionn (TName "strings"),DataTypeMemberConstructor "identifier-tagged" [TName "identifiers",TName "values"] (Just [])])]

funcons = libFromList
    [("environments",NullaryFuncon stepEnvironments),("envs",NullaryFuncon stepEnvironments),("identifier-tagged",StrictFuncon stepIdentifier_tagged),("id-tagged",StrictFuncon stepIdentifier_tagged),("fresh-identifier",NullaryFuncon stepFresh_identifier),("initialise-binding",NonStrictFuncon stepInitialise_binding),("bind-value",StrictFuncon stepBind_value),("bind",StrictFuncon stepBind_value),("unbind",StrictFuncon stepUnbind),("bound-directly",StrictFuncon stepBound_directly),("bound-value",StrictFuncon stepBound_value),("bound",StrictFuncon stepBound_value),("closed",NonStrictFuncon stepClosed),("scope",PartiallyStrictFuncon [Strict,NonStrict] NonStrict stepScope),("accumulate",NonStrictFuncon stepAccumulate),("collateral",StrictFuncon stepCollateral),("bind-recursively",PartiallyStrictFuncon [Strict,NonStrict] NonStrict stepBind_recursively),("recursive",PartiallyStrictFuncon [Strict,NonStrict] NonStrict stepRecursive),("re-close",PartiallyStrictFuncon [Strict,NonStrict] NonStrict stepRe_close),("bind-to-forward-links",StrictFuncon stepBind_to_forward_links),("set-forward-links",StrictFuncon stepSet_forward_links),("identifiers",NullaryFuncon stepIdentifiers),("ids",NullaryFuncon stepIdentifiers)]

environments_ = FName "environments"
envs_ = FName "environments"
stepEnvironments = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "maps" [TName "identifiers",TSortSeq (TName "values") QuestionMarkOp]) env

identifier_tagged_ fargs = FApp "identifier-tagged" (fargs)
id_tagged_ fargs = FApp "identifier-tagged" (fargs)
stepIdentifier_tagged fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1",VPMetaVar "_X2"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X2") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 100)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 102)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 45)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 103)]),FValue (ADTVal "unicode-character" [FValue (Int 103)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 100)])])),TVar "_X1",TVar "_X2"]) env

fresh_identifier_ = FName "fresh-identifier"
stepFresh_identifier = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "identifier-tagged" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 103)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 100)])])),TName "fresh-atom"]) env

initialise_binding_ fargs = FApp "initialise-binding" (fargs)
stepInitialise_binding fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "X"] env
            rewriteTermTo (TApp "initialise-linking" [TApp "initialise-generating" [TApp "closed" [TVar "X"]]]) env

bind_value_ fargs = FApp "bind-value" (fargs)
bind_ fargs = FApp "bind-value" (fargs)
stepBind_value fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "I") (TName "identifiers"),VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TMap [TBinding (TVar "I") (TVar "V")]) env

unbind_ fargs = FApp "unbind" (fargs)
stepUnbind fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "I") (TName "identifiers")] env
            rewriteTermTo (TMap [TBinding (TVar "I") (TSeq [])]) env

bound_directly_ fargs = FApp "bound-directly" (fargs)
stepBound_directly fargs =
    evalRules [] [step1,step2]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "I") (TName "identifiers")] env
            env <- getInhPatt "environment" [VPMetaVar "Rho"] env
            env <- lifted_sideCondition (SCPatternMatch (TApp "lookup" [TVar "Rho",TVar "I"]) [VPAnnotated (VPMetaVar "V") (TName "values")]) env
            stepTermTo (TVar "V") env
          step2 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "I") (TName "identifiers")] env
            env <- getInhPatt "environment" [VPMetaVar "Rho"] env
            env <- lifted_sideCondition (SCPatternMatch (TApp "lookup" [TVar "Rho",TVar "I"]) []) env
            stepTermTo (TName "fail") env

bound_value_ fargs = FApp "bound-value" (fargs)
bound_ fargs = FApp "bound-value" (fargs)
stepBound_value fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "I") (TName "identifiers")] env
            rewriteTermTo (TApp "follow-if-link" [TApp "bound-directly" [TVar "I"]]) env

closed_ fargs = FApp "closed" (fargs)
stepClosed fargs =
    evalRules [rewrite1] [step1]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "V") (TName "values")] env
            rewriteTermTo (TVar "V") env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getInhPatt "environment" [VPWildCard] env
            env <- withInhTerm "environment" (TApp "map" []) env (premise (TVar "X") [PMetaVar "X'"] env)
            stepTermTo (TApp "closed" [TVar "X'"]) env

scope_ fargs = FApp "scope" (fargs)
stepScope fargs =
    evalRules [rewrite1] [step1]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated PWildCard (TName "environments"),PAnnotated (PMetaVar "V") (TName "values")] env
            rewriteTermTo (TVar "V") env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PAnnotated (PMetaVar "Rho1") (TName "environments"),PMetaVar "X"] env
            env <- getInhPatt "environment" [VPMetaVar "Rho0"] env
            env <- withInhTerm "environment" (TApp "map-override" [TVar "Rho1",TVar "Rho0"]) env (premise (TVar "X") [PMetaVar "X'"] env)
            stepTermTo (TApp "scope" [TVar "Rho1",TVar "X'"]) env

accumulate_ fargs = FApp "accumulate" (fargs)
stepAccumulate fargs =
    evalRules [rewrite1,rewrite2,rewrite3,rewrite4] [step1]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "Rho1") (TName "environments"),PMetaVar "D2"] env
            rewriteTermTo (TApp "scope" [TVar "Rho1",TApp "map-override" [TVar "D2",TVar "Rho1"]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- fsMatch fargs [] env
            rewriteTermTo (TApp "map" []) env
          rewrite3 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "D1"] env
            rewriteTermTo (TVar "D1") env
          rewrite4 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "D1",PMetaVar "D2",PSeqVar "D+" PlusOp] env
            rewriteTermTo (TApp "accumulate" [TVar "D1",TApp "accumulate" [TVar "D2",TVar "D+"]]) env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "D1",PMetaVar "D2"] env
            env <- premise (TVar "D1") [PMetaVar "D1'"] env
            stepTermTo (TApp "accumulate" [TVar "D1'",TVar "D2"]) env

collateral_ fargs = FApp "collateral" (fargs)
stepCollateral fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPSeqVar "Rho*" StarOp) (TSortSeq (TName "environments") StarOp)] env
            rewriteTermTo (TApp "checked" [TApp "map-unite" [TVar "Rho*"]]) env

bind_recursively_ fargs = FApp "bind-recursively" (fargs)
stepBind_recursively fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "I") (TName "identifiers"),PMetaVar "E"] env
            rewriteTermTo (TApp "recursive" [TSet [TVar "I"],TApp "bind-value" [TVar "I",TVar "E"]]) env

recursive_ fargs = FApp "recursive" (fargs)
stepRecursive fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "SI") (TApp "sets" [TName "identifiers"]),PMetaVar "D"] env
            rewriteTermTo (TApp "re-close" [TApp "bind-to-forward-links" [TVar "SI"],TVar "D"]) env

re_close_ fargs = FApp "re-close" (fargs)
stepRe_close fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "M") (TApp "maps" [TName "identifiers",TName "links"]),PMetaVar "D"] env
            rewriteTermTo (TApp "accumulate" [TApp "scope" [TVar "M",TVar "D"],TApp "sequential" [TApp "set-forward-links" [TVar "M"],TApp "map" []]]) env

bind_to_forward_links_ fargs = FApp "bind-to-forward-links" (fargs)
stepBind_to_forward_links fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SI") (TApp "sets" [TName "identifiers"])] env
            rewriteTermTo (TApp "map-unite" [TApp "interleave-map" [TApp "bind-value" [TName "given",TApp "fresh-link" [TName "values"]],TApp "set-elements" [TVar "SI"]]]) env

set_forward_links_ fargs = FApp "set-forward-links" (fargs)
stepSet_forward_links fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "M") (TApp "maps" [TName "identifiers",TName "links"])] env
            rewriteTermTo (TApp "effect" [TApp "interleave-map" [TApp "set-link" [TApp "map-lookup" [TVar "M",TName "given"],TApp "bound-value" [TName "given"]],TApp "set-elements" [TApp "map-domain" [TVar "M"]]]]) env

identifiers_ = FName "identifiers"
stepIdentifiers = rewriteType "identifiers" []