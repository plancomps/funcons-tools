-- GeNeRaTeD fOr: /home/thomas/repos/plancomps/CBS-beta/Unstable-Funcons-beta/Computations/Threads/Synchronising/Synchronising.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Threads.Synchronising.Synchronising where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("syncs",DataTypeMemberss "syncs" [] [DataTypeMemberConstructor "sync" [TName "sync-feature-maps"] (Just [])]),("sync-features",DataTypeMemberss "sync-features" [] [DataTypeMemberConstructor "sync-waiting-list" [] (Just []),DataTypeMemberConstructor "sync-held" [] (Just []),DataTypeMemberConstructor "sync-holder" [] (Just []),DataTypeMemberConstructor "sync-count" [] (Just [])])]

funcons = libFromList
    [("sync",StrictFuncon stepSync),("sync-create",StrictFuncon stepSync_create),("sync-feature",StrictFuncon stepSync_feature),("is-sync-feature",StrictFuncon stepIs_sync_feature),("sync-waiting-list",NullaryFuncon stepSync_waiting_list),("sync-held",NullaryFuncon stepSync_held),("sync-holder",NullaryFuncon stepSync_holder),("sync-count",NullaryFuncon stepSync_count),("sync-feature-maps",NullaryFuncon stepSync_feature_maps),("sync-feature-create",StrictFuncon stepSync_feature_create),("sync-waiting-list-add",StrictFuncon stepSync_waiting_list_add),("sync-waiting-list-head-remove",StrictFuncon stepSync_waiting_list_head_remove),("syncs",NullaryFuncon stepSyncs),("sync-features",NullaryFuncon stepSync_features)]

sync_ fargs = FApp "sync" (fargs)
stepSync fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 121)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 99)])])),TVar "_X1"]) env

sync_create_ fargs = FApp "sync-create" (fargs)
stepSync_create fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPSeqVar "M+" PlusOp) (TSortSeq (TName "sync-feature-maps") PlusOp)] env
            rewriteTermTo (TApp "sync" [TApp "checked" [TApp "map-unite" [TVar "M+"]]]) env

sync_feature_ fargs = FApp "sync-feature" (fargs)
stepSync_feature fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "sync" [VPAnnotated (VPMetaVar "SFM") (TName "sync-feature-maps")],VPAnnotated (VPMetaVar "SF") (TName "sync-features")] env
            rewriteTermTo (TApp "checked" [TApp "map-lookup" [TVar "SFM",TVar "SF"]]) env

is_sync_feature_ fargs = FApp "is-sync-feature" (fargs)
stepIs_sync_feature fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "sync" [VPAnnotated (VPMetaVar "SFM") (TName "sync-feature-maps")],VPAnnotated (VPMetaVar "SF") (TName "sync-features")] env
            rewriteTermTo (TApp "is-in-set" [TVar "SF",TApp "dom" [TVar "SFM"]]) env

sync_waiting_list_ = FName "sync-waiting-list"
stepSync_waiting_list = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 121)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 45)]),FValue (ADTVal "unicode-character" [FValue (Int 119)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 103)]),FValue (ADTVal "unicode-character" [FValue (Int 45)]),FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]))]) env

sync_held_ = FName "sync-held"
stepSync_held = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 121)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 45)]),FValue (ADTVal "unicode-character" [FValue (Int 104)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 100)])]))]) env

sync_holder_ = FName "sync-holder"
stepSync_holder = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 121)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 45)]),FValue (ADTVal "unicode-character" [FValue (Int 104)]),FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 100)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 114)])]))]) env

sync_count_ = FName "sync-count"
stepSync_count = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 121)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 45)]),FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 117)]),FValue (ADTVal "unicode-character" [FValue (Int 110)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]))]) env

sync_feature_maps_ = FName "sync-feature-maps"
stepSync_feature_maps = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "maps" [TName "sync-features",TName "values"]) env

sync_feature_create_ fargs = FApp "sync-feature-create" (fargs)
stepSync_feature_create fargs =
    evalRules [rewrite1,rewrite2,rewrite3,rewrite4] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "sync-waiting-list" []] env
            rewriteTermTo (TMap [TBinding (TName "sync-waiting-list") (TApp "allocate-initialised-variable" [TApp "lists" [TName "values"],TApp "list" []])]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "sync-held" []] env
            rewriteTermTo (TMap [TBinding (TName "sync-held") (TApp "allocate-initialised-variable" [TName "booleans",TName "false"])]) env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "sync-holder" []] env
            rewriteTermTo (TMap [TBinding (TName "sync-holder") (TApp "allocate-variable" [TName "thread-ids"])]) env
          rewrite4 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "sync-count" []] env
            rewriteTermTo (TMap [TBinding (TName "sync-count") (TApp "allocate-initialised-variable" [TName "nats",TFuncon (FValue (Nat 0))])]) env

sync_waiting_list_add_ fargs = FApp "sync-waiting-list-add" (fargs)
stepSync_waiting_list_add fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs"),VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"],TApp "list-append" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"]],TApp "list" [TVar "V"]]]) env

sync_waiting_list_head_remove_ fargs = FApp "sync-waiting-list-head-remove" (fargs)
stepSync_waiting_list_head_remove fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "give" [TApp "checked" [TApp "list-head" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"]]]],TApp "sequential" [TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"],TApp "checked" [TApp "list-tail" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"]]]]],TName "given"]]) env

syncs_ = FName "syncs"
stepSyncs = rewriteType "syncs" []

sync_features_ = FName "sync-features"
stepSync_features = rewriteType "sync-features" []