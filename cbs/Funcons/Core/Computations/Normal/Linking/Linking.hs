-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Computations/Normal/Linking/Linking.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Normal.Linking.Linking where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("links",DataTypeMemberss "links" [] [DataTypeMemberConstructor "link" [TName "variables"] (Just [])])]

funcons = libFromList
    [("link",StrictFuncon stepLink),("initialise-linking",NonStrictFuncon stepInitialise_linking),("fresh-link",NonStrictFuncon stepFresh_link),("fresh-initialised-link",PartiallyStrictFuncon [NonStrict,Strict] NonStrict stepFresh_initialised_link),("fresh-init-link",PartiallyStrictFuncon [NonStrict,Strict] NonStrict stepFresh_initialised_link),("set-link",StrictFuncon stepSet_link),("follow-link",StrictFuncon stepFollow_link),("follow-if-link",StrictFuncon stepFollow_if_link),("links",NullaryFuncon stepLinks)]

link_ fargs = FApp "link" (fargs)
stepLink fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 107)])])),TVar "_X1"]) env

initialise_linking_ fargs = FApp "initialise-linking" (fargs)
stepInitialise_linking fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "X"] env
            rewriteTermTo (TApp "initialise-storing" [TVar "X"]) env

fresh_link_ fargs = FApp "fresh-link" (fargs)
stepFresh_link fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "T"] env
            rewriteTermTo (TApp "link" [TApp "allocate-variable" [TVar "T"]]) env

fresh_initialised_link_ fargs = FApp "fresh-initialised-link" (fargs)
fresh_init_link_ fargs = FApp "fresh-initialised-link" (fargs)
stepFresh_initialised_link fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "T",PAnnotated (PMetaVar "V") (TVar "T")] env
            rewriteTermTo (TApp "link" [TApp "allocate-initialised-variable" [TVar "T",TVar "V"]]) env

set_link_ fargs = FApp "set-link" (fargs)
stepSet_link fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "link" [VPAnnotated (VPMetaVar "Var") (TName "variables")],VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TApp "initialise-variable" [TVar "Var",TVar "V"]) env

follow_link_ fargs = FApp "follow-link" (fargs)
stepFollow_link fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "link" [VPAnnotated (VPMetaVar "Var") (TName "variables")]] env
            rewriteTermTo (TApp "assigned" [TVar "Var"]) env

follow_if_link_ fargs = FApp "follow-if-link" (fargs)
stepFollow_if_link fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "link" [VPAnnotated (VPMetaVar "Var") (TName "variables")]] env
            rewriteTermTo (TApp "assigned" [TVar "Var"]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "V") (TSortComplement (TName "links"))] env
            rewriteTermTo (TVar "V") env

links_ = FName "links"
stepLinks = rewriteType "links" []