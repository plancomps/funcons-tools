-- GeNeRaTeD fOr: /home/thomas/repos/plancomps/CBS-beta/Unstable-Funcons-beta/Computations/Threads/Synchronising/Notifications/Notifications.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Threads.Synchronising.Notifications.Notifications where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("barrier-create",StrictFuncon stepBarrier_create),("barrier-sync",StrictFuncon stepBarrier_sync),("barrier-sync-else-wait",StrictFuncon stepBarrier_sync_else_wait),("condition-create",NullaryFuncon stepCondition_create),("condition-wait",StrictFuncon stepCondition_wait),("condition-wait-with-lock",StrictFuncon stepCondition_wait_with_lock),("condition-notify-all",StrictFuncon stepCondition_notify_all),("condition-notify-first",StrictFuncon stepCondition_notify_first),("rendezvous-create",StrictFuncon stepRendezvous_create),("rendezvous-sync",StrictFuncon stepRendezvous_sync),("rendezvous-sync-else-wait",StrictFuncon stepRendezvous_sync_else_wait),("rendezvous-waits",NullaryFuncon stepRendezvous_waits),("is-rendezvous-match",StrictFuncon stepIs_rendezvous_match),("rendezvous-first-match-thread",StrictFuncon stepRendezvous_first_match_thread),("rendezvous-first-match-drop",StrictFuncon stepRendezvous_first_match_drop)]

barrier_create_ fargs = FApp "barrier-create" (fargs)
stepBarrier_create fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "N") (TName "pos-ints")] env
            rewriteTermTo (TApp "give" [TApp "sync-create" [TApp "sync-feature-create" [TName "sync-waiting-list"],TApp "sync-feature-create" [TName "sync-count"]],TApp "sequential" [TApp "assign" [TApp "sync-feature" [TName "given",TName "sync-count"],TVar "N"],TName "given"]]) env

barrier_sync_ fargs = FApp "barrier-sync" (fargs)
stepBarrier_sync fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "give" [TApp "sync-feature" [TVar "SY",TName "sync-count"],TApp "else" [TApp "check-true" [TApp "is-equal" [TApp "assigned" [TName "given"],TFuncon (FValue (Nat 0))]],TApp "sequential" [TApp "check-true" [TApp "is-equal" [TApp "assigned" [TName "given"],TFuncon (FValue (Nat 1))]],TApp "assign" [TName "given",TFuncon (FValue (Nat 0))],TApp "thread-resume" [TApp "list-elements" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"]]]],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"],TApp "list" []]]]]]) env

barrier_sync_else_wait_ fargs = FApp "barrier-sync-else-wait" (fargs)
stepBarrier_sync_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "else" [TApp "barrier-sync" [TVar "SY"],TApp "sequential" [TApp "sync-waiting-list-add" [TVar "SY",TName "current-thread"],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-count"],TApp "checked" [TApp "nat-pred" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-count"]]]]],TApp "thread-suspend" [TName "current-thread"]]]]) env

condition_create_ = FName "condition-create"
stepCondition_create = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "sync-create" [TApp "sync-feature-create" [TName "sync-waiting-list"]]) env

condition_wait_ fargs = FApp "condition-wait" (fargs)
stepCondition_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "sync-waiting-list-add" [TVar "SY",TName "current-thread"],TApp "thread-suspend" [TName "current-thread"]]]) env

condition_wait_with_lock_ fargs = FApp "condition-wait-with-lock" (fargs)
stepCondition_wait_with_lock fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs"),VPAnnotated (VPMetaVar "L") (TName "syncs")] env
            rewriteTermTo (TApp "sequential" [TApp "thread-atomic" [TApp "sequential" [TApp "exclusive-lock-release" [TVar "L"],TApp "sync-waiting-list-add" [TVar "SY",TName "current-thread"],TApp "thread-suspend" [TName "current-thread"]]],TApp "exclusive-lock-sync-else-wait" [TVar "L"]]) env

condition_notify_all_ fargs = FApp "condition-notify-all" (fargs)
stepCondition_notify_all fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "sequential" [TApp "thread-resume" [TApp "list-elements" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"]]]],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"],TApp "list" []]]]) env

condition_notify_first_ fargs = FApp "condition-notify-first" (fargs)
stepCondition_notify_first fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "give" [TApp "sync-waiting-list-head-remove" [TVar "SY"],TApp "thread-resume" [TName "given"]]]) env

rendezvous_create_ fargs = FApp "rendezvous-create" (fargs)
stepRendezvous_create fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "N") (TName "pos-ints")] env
            rewriteTermTo (TApp "give" [TApp "sync-create" [TApp "sync-feature-create" [TName "sync-waiting-list"],TApp "sync-feature-create" [TName "sync-count"]],TApp "sequential" [TApp "assign" [TApp "sync-feature" [TName "given",TName "sync-count"],TVar "N"],TName "given"]]) env

rendezvous_sync_ fargs = FApp "rendezvous-sync" (fargs)
stepRendezvous_sync fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs"),VPAnnotated (VPMetaVar "V") (TName "ground-values")] env
            rewriteTermTo (TApp "give" [TApp "assigned" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"]],TApp "sequential" [TApp "check-true" [TApp "is-rendezvous-match" [TName "given",TVar "V"]],TApp "assign" [TApp "sync-feature" [TVar "SY",TName "sync-waiting-list"],TApp "rendezvous-first-match-drop" [TName "given",TVar "V"]],TApp "thread-resume" [TApp "rendezvous-first-match-thread" [TName "given",TVar "V"]]]]) env

rendezvous_sync_else_wait_ fargs = FApp "rendezvous-sync-else-wait" (fargs)
stepRendezvous_sync_else_wait fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "SY") (TName "syncs"),VPAnnotated (VPMetaVar "V") (TName "ground-values")] env
            rewriteTermTo (TApp "thread-atomic" [TApp "else" [TApp "rendezvous-sync" [TVar "SY",TVar "V"],TApp "sequential" [TApp "sync-waiting-list-add" [TVar "SY",TApp "tuple" [TVar "V",TName "current-thread"]],TApp "thread-suspend" [TName "current-thread"]]]]) env

rendezvous_waits_ = FName "rendezvous-waits"
stepRendezvous_waits = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "tuples" [TName "ground-values",TName "thread-ids"]) env

is_rendezvous_match_ fargs = FApp "is-rendezvous-match" (fargs)
stepIs_rendezvous_match fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),PADT "tuple" [VPMetaVar "V'",VPMetaVar "TI"],VPSeqVar "P*" StarOp],VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TApp "if-true-else" [TApp "is-equal" [TVar "V'",TVar "V"],TName "true",TApp "is-rendezvous-match" [TApp "list" [TVar "P*"],TVar "V"]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])])],VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TName "false") env

rendezvous_first_match_thread_ fargs = FApp "rendezvous-first-match-thread" (fargs)
stepRendezvous_first_match_thread fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),PADT "tuple" [VPMetaVar "V'",VPMetaVar "TI"],VPSeqVar "P*" StarOp],VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TApp "if-true-else" [TApp "is-equal" [TVar "V'",TVar "V"],TVar "TI",TApp "rendezvous-first-match-thread" [TApp "list" [TVar "P*"],TVar "V"]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])])],VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TName "fail") env

rendezvous_first_match_drop_ fargs = FApp "rendezvous-first-match-drop" (fargs)
stepRendezvous_first_match_drop fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),PADT "tuple" [VPMetaVar "V'",VPMetaVar "TI"],VPSeqVar "P*" StarOp],VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TApp "if-true-else" [TApp "is-equal" [TVar "V'",TVar "V"],TApp "list" [TVar "P*"],TApp "cons" [TApp "tuple" [TVar "V'",TVar "TI"],TApp "rendezvous-first-match-drop" [TApp "list" [TVar "P*"],TVar "V"]]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])])],VPAnnotated (VPMetaVar "V") (TName "values")] env
            rewriteTermTo (TName "fail") env