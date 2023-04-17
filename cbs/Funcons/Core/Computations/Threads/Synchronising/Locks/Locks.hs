-- GeNeRaTeD fOr: /home/thomas/repos/plancomps/CBS-beta/Unstable-Funcons-beta/Computations/Threads/Synchronising/Locks/Locks.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Threads.Synchronising.Locks.Locks where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("is-exclusive-lock-holder",StrictFuncon stepIs_exclusive_lock_holder),("spin-lock-create",NullaryFuncon stepSpin_lock_create),("spin-lock-sync",StrictFuncon stepSpin_lock_sync),("spin-lock-release",StrictFuncon stepSpin_lock_release),("exclusive-lock-create",NullaryFuncon stepExclusive_lock_create),("exclusive-lock-sync",StrictFuncon stepExclusive_lock_sync),("exclusive-lock-sync-else-wait",StrictFuncon stepExclusive_lock_sync_else_wait),("exclusive-lock-release",StrictFuncon stepExclusive_lock_release),("reentrant-lock-create",NullaryFuncon stepReentrant_lock_create),("reentrant-lock-sync",StrictFuncon stepReentrant_lock_sync),("reentrant-lock-sync-else-wait",StrictFuncon stepReentrant_lock_sync_else_wait),("reentrant-lock-release",StrictFuncon stepReentrant_lock_release),("reentrant-lock-exit",StrictFuncon stepReentrant_lock_exit),("semaphore-create",StrictFuncon stepSemaphore_create),("semaphore-sync",StrictFuncon stepSemaphore_sync),("semaphore-sync-else-wait",StrictFuncon stepSemaphore_sync_else_wait),("semaphore-release",StrictFuncon stepSemaphore_release),("rw-lock-create",NullaryFuncon stepRw_lock_create),("rw-lock-sync-exclusive",StrictFuncon stepRw_lock_sync_exclusive),("rw-lock-sync-shared",StrictFuncon stepRw_lock_sync_shared),("rw-lock-sync-exclusive-else-wait",StrictFuncon stepRw_lock_sync_exclusive_else_wait),("rw-lock-sync-shared-else-wait",StrictFuncon stepRw_lock_sync_shared_else_wait),("rw-lock-release-exclusive",StrictFuncon stepRw_lock_release_exclusive),("rw-lock-release-shared",StrictFuncon stepRw_lock_release_shared),("rw-lock-sync",StrictFuncon stepRw_lock_sync),("rw-lock-sync-all-shared",StrictFuncon stepRw_lock_sync_all_shared)]

is_exclusive_lock_holder_ fargs = FApp "is-exclusive-lock-holder" (fargs)
stepIs_exclusive_lock_holder fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "is-equal" [TName "current-thread",TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-holder"]]]) env

spin_lock_create_ = FName "spin-lock-create"
stepSpin_lock_create = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "sync-create" [TApp "sync-feature-create" [TName "sync-held"],TApp "sync-feature-create" [TName "sync-holder"]]) env

spin_lock_sync_ fargs = FApp "spin-lock-sync" (fargs)
stepSpin_lock_sync fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "check-true" [TApp "not" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-held"]]]],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-held"],TName "true"],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-holder"],TName "current-thread"]]]) env

spin_lock_release_ fargs = FApp "spin-lock-release" (fargs)
stepSpin_lock_release fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "check-true" [TApp "is-exclusive-lock-holder" [TVar "SY"]],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-held"],TName "false"],TApp "un-assign" [TApp "sync-feature" [TVar "SY",TName "sync-holder"]]]]) env

exclusive_lock_create_ = FName "exclusive-lock-create"
stepExclusive_lock_create = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "sync-create" [TApp "sync-feature-create" [TName "sync-waiting-list"],TApp "sync-feature-create" [TName "sync-held"],TApp "sync-feature-create" [TName "sync-holder"]]) env

exclusive_lock_sync_ fargs = FApp "exclusive-lock-sync" (fargs)
stepExclusive_lock_sync fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "check-true" [TApp "not" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-held"]]]],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-held"],TName "true"],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-holder"],TName "current-thread"]]]) env

exclusive_lock_sync_else_wait_ fargs = FApp "exclusive-lock-sync-else-wait" (fargs)
stepExclusive_lock_sync_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "else" [TApp "exclusive-lock-sync" [TVar "SY"],TApp "sequential" [TApp "sync-waiting-list-add" [TVar "SY",TName "current-thread"],TApp "thread-suspend" [TName "current-thread"]]]]) env

exclusive_lock_release_ fargs = FApp "exclusive-lock-release" (fargs)
stepExclusive_lock_release fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "check-true" [TApp "is-exclusive-lock-holder" [TVar "SY"]],TApp "if-true-else" [TApp "is-equal" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"]],TApp "list" []],TApp "sequential" [TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-held"],TName "false"],TApp "un-assign" [TApp "sync-feature" [TVar "SY",TName "sync-holder"]]],TApp "give" [TApp "sync-waiting-list-head-remove" [TVar "SY"],TApp "sequential" [TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-holder"],TName "given"],TApp "thread-resume" [TName "given"]]]]]]) env

reentrant_lock_create_ = FName "reentrant-lock-create"
stepReentrant_lock_create = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "sync-create" [TApp "sync-feature-create" [TName "sync-waiting-list"],TApp "sync-feature-create" [TName "sync-held"],TApp "sync-feature-create" [TName "sync-holder"],TApp "sync-feature-create" [TName "sync-count"]]) env

reentrant_lock_sync_ fargs = FApp "reentrant-lock-sync" (fargs)
stepReentrant_lock_sync fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "else" [TApp "sequential" [TApp "check-true" [TApp "not" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-held"]]]],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-held"],TName "true"],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-holder"],TName "current-thread"],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-count"],TFuncon (FValue (Nat 0))]],TApp "sequential" [TApp "check-true" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-held"]]],TApp "check-true" [TApp "is-exclusive-lock-holder" [TVar "SY"]],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-count"],TApp "nat-succ" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-count"]]]]]]]) env

reentrant_lock_sync_else_wait_ fargs = FApp "reentrant-lock-sync-else-wait" (fargs)
stepReentrant_lock_sync_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "else" [TApp "reentrant-lock-sync" [TVar "SY"],TApp "sequential" [TApp "sync-waiting-list-add" [TVar "SY",TName "current-thread"],TApp "thread-suspend" [TName "current-thread"]]]]) env

reentrant_lock_release_ fargs = FApp "reentrant-lock-release" (fargs)
stepReentrant_lock_release fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "check-true" [TApp "is-exclusive-lock-holder" [TVar "SY"]],TApp "if-true-else" [TApp "is-equal" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"]],TApp "list" []],TApp "sequential" [TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-held"],TName "false"],TApp "un-assign" [TApp "sync-feature" [TVar "SY",TName "sync-holder"]],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-count"],TFuncon (FValue (Nat 0))]],TApp "give" [TApp "sync-waiting-list-head-remove" [TVar "SY"],TApp "sequential" [TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-holder"],TName "given"],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-count"],TFuncon (FValue (Nat 0))],TApp "thread-resume" [TName "given"]]]]]]) env

reentrant_lock_exit_ fargs = FApp "reentrant-lock-exit" (fargs)
stepReentrant_lock_exit fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "check-true" [TApp "is-exclusive-lock-holder" [TVar "SY"]],TApp "give" [TApp "sync-feature" [TVar "SY",TName "sync-count"],TApp "if-true-else" [TApp "is-greater" [TApp "assigned" [TName "given"],TFuncon (FValue (Nat 0))],TApp "assign" [TName "given",TApp "checked" [TApp "nat-pred" [TApp "assigned" [TName "given"]]]],TApp "reentrant-lock-release" [TVar "SY"]]]]]) env

semaphore_create_ fargs = FApp "semaphore-create" (fargs)
stepSemaphore_create fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "N") (TName "pos-ints")] env
            rewriteTermTo (TApp "give" [TApp "sync-create" [TApp "sync-feature-create" [TName "sync-waiting-list"],TApp "sync-feature-create" [TName "sync-count"]],TApp "sequential" [TApp "assign" [TApp "sync-feature" [TName "given",TName "sync-count"],TVar "N"],TName "given"]]) env

semaphore_sync_ fargs = FApp "semaphore-sync" (fargs)
stepSemaphore_sync fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "check-true" [TApp "is-greater" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-count"]],TFuncon (FValue (Nat 0))]],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-count"],TApp "checked" [TApp "nat-pred" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-count"]]]]]]]) env

semaphore_sync_else_wait_ fargs = FApp "semaphore-sync-else-wait" (fargs)
stepSemaphore_sync_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "else" [TApp "semaphore-sync" [TVar "SY"],TApp "sequential" [TApp "sync-waiting-list-add" [TVar "SY",TName "current-thread"],TApp "thread-suspend" [TName "current-thread"]]]]) env

semaphore_release_ fargs = FApp "semaphore-release" (fargs)
stepSemaphore_release fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "if-true-else" [TApp "is-equal" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"]],TApp "list" []],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-count"],TApp "nat-succ" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-count"]]]],TApp "give" [TApp "sync-waiting-list-head-remove" [TVar "SY"],TApp "thread-resume" [TName "given"]]]]) env

rw_lock_create_ = FName "rw-lock-create"
stepRw_lock_create = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "give" [TApp "sync-create" [TApp "sync-feature-create" [TName "sync-waiting-list"],TApp "sync-feature-create" [TName "sync-held"],TApp "sync-feature-create" [TName "sync-count"]],TApp "sequential" [TApp "assign" [TApp "sync-feature" [TName "given",TName "sync-count"],TFuncon (FValue (Nat 0))],TName "given"]]) env

rw_lock_sync_exclusive_ fargs = FApp "rw-lock-sync-exclusive" (fargs)
stepRw_lock_sync_exclusive fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "check-true" [TApp "and" [TApp "not" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-held"]]],TApp "is-equal" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-count"]],TFuncon (FValue (Nat 0))]]],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-held"],TName "true"]]]) env

rw_lock_sync_shared_ fargs = FApp "rw-lock-sync-shared" (fargs)
stepRw_lock_sync_shared fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "check-true" [TApp "not" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-held"]]]],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-count"],TApp "nat-succ" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-count"]]]]]]) env

rw_lock_sync_exclusive_else_wait_ fargs = FApp "rw-lock-sync-exclusive-else-wait" (fargs)
stepRw_lock_sync_exclusive_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "else" [TApp "rw-lock-sync-exclusive" [TVar "SY"],TApp "sequential" [TApp "sync-waiting-list-add" [TVar "SY",TApp "tuple" [TName "current-thread",TName "false"]],TApp "thread-suspend" [TName "current-thread"]]]]) env

rw_lock_sync_shared_else_wait_ fargs = FApp "rw-lock-sync-shared-else-wait" (fargs)
stepRw_lock_sync_shared_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "else" [TApp "rw-lock-sync-shared" [TVar "SY"],TApp "sequential" [TApp "sync-waiting-list-add" [TVar "SY",TApp "tuple" [TName "current-thread",TName "true"]],TApp "thread-suspend" [TName "current-thread"]]]]) env

rw_lock_release_exclusive_ fargs = FApp "rw-lock-release-exclusive" (fargs)
stepRw_lock_release_exclusive fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "check-true" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-held"]]],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-held"],TName "false"],TApp "rw-lock-sync" [TVar "SY"]]]) env

rw_lock_release_shared_ fargs = FApp "rw-lock-release-shared" (fargs)
stepRw_lock_release_shared fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-count"],TApp "checked" [TApp "nat-pred" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-count"]]]]],TApp "if-true-else" [TApp "is-equal" [TFuncon (FValue (Nat 0)),TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-count"]]],TApp "rw-lock-sync" [TVar "SY"],TName "null-value"]]]) env

rw_lock_sync_ fargs = FApp "rw-lock-sync" (fargs)
stepRw_lock_sync fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "if-true-else" [TApp "is-equal" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"]],TApp "list" []],TName "null-value",TApp "give" [TApp "sync-waiting-list-head-remove" [TVar "SY"],TApp "sequential" [TApp "thread-resume" [TApp "first" [TApp "tuple-elements" [TName "given"]]],TApp "if-true-else" [TApp "second" [TApp "tuple-elements" [TName "given"]],TApp "sequential" [TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-count"],TApp "nat-succ" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-count"]]]],TApp "rw-lock-sync-all-shared" [TVar "SY"]],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-held"],TName "true"]]]]]) env

rw_lock_sync_all_shared_ fargs = FApp "rw-lock-sync-all-shared" (fargs)
stepRw_lock_sync_all_shared fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"],TApp "list" [TApp "left-to-right-filter" [TApp "if-true-else" [TApp "second" [TApp "tuple-elements" [TName "given"]],TApp "sequential" [TApp "thread-resume" [TApp "first" [TApp "tuple-elements" [TName "given"]]],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-count"],TApp "nat-succ" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-count"]]]],TName "false"],TName "true"],TApp "list-elements" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"]]]]]]) env