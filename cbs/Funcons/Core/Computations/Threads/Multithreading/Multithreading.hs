-- GeNeRaTeD fOr: /home/thomas/repos/plancomps/CBS-beta/Unstable-Funcons-beta/Computations/Threads/Multithreading/Multithreading.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Threads.Multithreading.Multithreading where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    [("thread-ids",DataTypeMemberss "thread-ids" [] [DataTypeMemberConstructor "thread-id" [TName "atoms"] (Just [])]),("threads",DataTypeMemberss "threads" [] [DataTypeMemberConstructor "thread" [TApp "thunks" [TName "values"],TSortSeq (TApp "lists" [TName "thread-ids"]) QuestionMarkOp] (Just [])]),("thread-preemtibilities",DataTypeMemberss "thread-preemtibilities" [] [DataTypeMemberConstructor "thread-preemptible" [] (Just []),DataTypeMemberConstructor "thread-cooperative" [] (Just [])])]

funcons = libFromList
    [("thread-id",StrictFuncon stepThread_id),("thread",StrictFuncon stepThread),("thread-joinable",StrictFuncon stepThread_joinable),("thread-detached",StrictFuncon stepThread_detached),("is-some-thread-active",NullaryFuncon stepIs_some_thread_active),("is-some-thread-suspended",NullaryFuncon stepIs_some_thread_suspended),("initialise-multithreading",NullaryFuncon stepInitialise_multithreading),("initialise-thread-map",NullaryFuncon stepInitialise_thread_map),("initialise-active-thread-set",NullaryFuncon stepInitialise_active_thread_set),("initialise-thread-stepping",NullaryFuncon stepInitialise_thread_stepping),("initialise-terminated-thread-map",NullaryFuncon stepInitialise_terminated_thread_map),("initialise-thread-schedule",NullaryFuncon stepInitialise_thread_schedule),("multithread",NonStrictFuncon stepMultithread),("thread-activate",StrictFuncon stepThread_activate),("thread-detach",StrictFuncon stepThread_detach),("current-thread",NullaryFuncon stepCurrent_thread),("thread-step",NullaryFuncon stepThread_step),("thread-atomic",NonStrictFuncon stepThread_atomic),("thread-yield",StrictFuncon stepThread_yield),("thread-spin",NonStrictFuncon stepThread_spin),("thread-suspend",StrictFuncon stepThread_suspend),("thread-resume",StrictFuncon stepThread_resume),("thread-terminate",StrictFuncon stepThread_terminate),("is-thread-terminated",StrictFuncon stepIs_thread_terminated),("thread-value",StrictFuncon stepThread_value),("thread-join",StrictFuncon stepThread_join),("thread-exterminate",StrictFuncon stepThread_exterminate),("update-thread-stepping",NullaryFuncon stepUpdate_thread_stepping),("update-thread-schedule",StrictFuncon stepUpdate_thread_schedule),("current-thread-schedule",NullaryFuncon stepCurrent_thread_schedule),("thread-preemptible",NullaryFuncon stepThread_preemptible),("thread-cooperative",NullaryFuncon stepThread_cooperative),("is-thread-preemptible",StrictFuncon stepIs_thread_preemptible),("thread-ids",NullaryFuncon stepThread_ids),("threads",NullaryFuncon stepThreads),("thread-preemtibilities",NullaryFuncon stepThread_preemtibilities)]

thread_id_ fargs = FApp "thread-id" (fargs)
stepThread_id fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1"] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 104)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 100)]),FValue (ADTVal "unicode-character" [FValue (Int 45)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 100)])])),TVar "_X1"]) env

thread_ fargs = FApp "thread" (fargs)
stepThread fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPMetaVar "_X1",VPSeqVar "_X2?" QuestionMarkOp] env
            env <- sideCondition (SCIsInSort (TVar "_X1") (TSortSeq (TName "values") QuestionMarkOp)) env
            env <- sideCondition (SCIsInSort (TVar "_X2?") (TSortSeq (TSortSeq (TName "values") QuestionMarkOp) QuestionMarkOp)) env
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 104)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 100)])])),TVar "_X1",TVar "_X2?"]) env

thread_joinable_ fargs = FApp "thread-joinable" (fargs)
stepThread_joinable fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "TH") (TApp "thunks" [TName "values"])] env
            rewriteTermTo (TApp "thread" [TVar "TH",TApp "list" []]) env

thread_detached_ fargs = FApp "thread-detached" (fargs)
stepThread_detached fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "TH") (TApp "thunks" [TName "values"])] env
            rewriteTermTo (TApp "thread" [TVar "TH"]) env

is_some_thread_active_ = FName "is-some-thread-active"
stepIs_some_thread_active = evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS"] env
            stepTermTo (TApp "not" [TApp "is-equal" [TVar "ATS",TSet []]]) env

is_some_thread_suspended_ = FName "is-some-thread-suspended"
stepIs_some_thread_suspended = evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS"] env
            env <- getMutPatt "thread-map" [VPMetaVar "TM"] env
            stepTermTo (TApp "not" [TApp "is-equal" [TVar "ATS",TApp "dom" [TVar "TM"]]]) env

initialise_multithreading_ = FName "initialise-multithreading"
stepInitialise_multithreading = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "sequential" [TName "initialise-thread-map",TName "initialise-active-thread-set",TName "initialise-thread-stepping",TName "initialise-terminated-thread-map",TName "initialise-thread-schedule"]) env

initialise_thread_map_ = FName "initialise-thread-map"
stepInitialise_thread_map = evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            putMutTerm "thread-map" (TApp "map" []) env
            stepTermTo (TName "null-value") env

initialise_active_thread_set_ = FName "initialise-active-thread-set"
stepInitialise_active_thread_set = evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            putMutTerm "active-thread-set" (TSet []) env
            stepTermTo (TName "null-value") env

initialise_thread_stepping_ = FName "initialise-thread-stepping"
stepInitialise_thread_stepping = evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            putMutTerm "thread-stepping" (TSeq []) env
            stepTermTo (TName "null-value") env

initialise_terminated_thread_map_ = FName "initialise-terminated-thread-map"
stepInitialise_terminated_thread_map = evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            putMutTerm "terminated-thread-map" (TApp "map" []) env
            stepTermTo (TName "null-value") env

initialise_thread_schedule_ = FName "initialise-thread-schedule"
stepInitialise_thread_schedule = evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            putMutTerm "thread-schedule" (TSet []) env
            stepTermTo (TName "null-value") env

multithread_ fargs = FApp "multithread" (fargs)
stepMultithread fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "X"] env
            rewriteTermTo (TApp "sequential" [TName "initialise-multithreading",TApp "give" [TApp "thread-activate" [TApp "thread-joinable" [TApp "thunk" [TApp "closure" [TVar "X"]]]],TApp "handle-abrupt" [TApp "sequential" [TApp "while-true" [TName "is-some-thread-active",TApp "sequential" [TName "update-thread-stepping",TName "thread-step"]],TApp "check" [TApp "not" [TName "is-some-thread-suspended"]],TApp "thread-value" [TName "given"]],TName "given"]]]) env

thread_activate_ fargs = FApp "thread-activate" (fargs)
stepThread_activate fargs =
    evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "THR") (TName "threads")] env
            env <- getMutPatt "thread-map" [VPMetaVar "TM"] env
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS"] env
            env <- premise (TApp "thread-id" [TName "fresh-atom"]) [PMetaVar "TI"] env
            env <- lifted_sideCondition (SCPatternMatch (TApp "map-unite" [TMap [TBinding (TVar "TI") (TVar "THR")],TVar "TM"]) [VPMetaVar "TM'"]) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "set-unite" [TSet [TVar "TI"],TVar "ATS"]) [VPMetaVar "ATS'"]) env
            putMutTerm "thread-map" (TVar "TM'") env
            putMutTerm "active-thread-set" (TVar "ATS'") env
            stepTermTo (TVar "TI") env

thread_detach_ fargs = FApp "thread-detach" (fargs)
stepThread_detach fargs =
    evalRules [] [step1,step2]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "TI") (TName "thread-ids")] env
            env <- getMutPatt "thread-map" [VPMetaVar "TM"] env
            env <- lifted_sideCondition (SCPatternMatch (TApp "map-lookup" [TVar "TM",TVar "TI"]) [PADT "thread" [VPMetaVar "TH",VPWildCard]]) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "map-override" [TMap [TBinding (TVar "TI") (TApp "thread" [TVar "TH"])],TVar "TM"]) [VPMetaVar "TM'"]) env
            putMutTerm "thread-map" (TVar "TM'") env
            stepTermTo (TName "null-value") env
          step2 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "TI") (TName "thread-ids")] env
            env <- getMutPatt "terminated-thread-map" [VPMetaVar "TMV"] env
            env <- lifted_sideCondition (SCEquality (TApp "is-in-set" [TVar "TI",TApp "dom" [TVar "TMV"]]) (TName "true")) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "map-delete" [TVar "TMV",TSet [TVar "TI"]]) [VPMetaVar "TMV'"]) env
            putMutTerm "terminated-thread-map" (TVar "TMV'") env
            stepTermTo (TName "null-value") env

current_thread_ = FName "current-thread"
stepCurrent_thread = evalRules [] [step1,step2]
    where step1 = do
            let env = emptyEnv
            env <- getMutPatt "thread-stepping" [VPMetaVar "TI"] env
            stepTermTo (TVar "TI") env
          step2 = do
            let env = emptyEnv
            env <- getMutPatt "thread-stepping" [] env
            stepTermTo (TName "fail") env

thread_step_ = FName "thread-step"
stepThread_step = evalRules [] [step1,step2,step3]
    where step1 = do
            let env = emptyEnv
            env <- getMutPatt "thread-stepping" [VPMetaVar "TI"] env
            env <- getMutPatt "thread-map" [VPMetaVar "TM"] env
            env <- lifted_sideCondition (SCPatternMatch (TApp "lookup" [TVar "TM",TVar "TI"]) [PADT "thread" [PADT "thunk" [PADT "abstraction" [VPMetaVar "X"]],VPSeqVar "L?" QuestionMarkOp]]) env
            putMutTerm "thread-stepping" (TVar "TI") env
            putMutTerm "thread-map" (TVar "TM") env
            env <- premise (TVar "X") [PMetaVar "X'"] env
            env <- getMutPatt "thread-stepping" [VPSeqVar "TI?" QuestionMarkOp] env
            env <- getMutPatt "thread-map" [VPMetaVar "TM'"] env
            env <- lifted_sideCondition (SCPatternMatch (TApp "if-true-else" [TApp "is-in-set" [TVar "TI",TApp "dom" [TVar "TM'"]],TApp "map-override" [TMap [TBinding (TVar "TI") (TApp "thread" [TApp "thunk" [TApp "abstraction" [TVar "X'"]],TVar "L?"])],TVar "TM'"],TVar "TM'"]) [VPMetaVar "TM''"]) env
            putMutTerm "thread-stepping" (TVar "TI?") env
            putMutTerm "thread-map" (TVar "TM''") env
            stepTermTo (TName "null-value") env
          step2 = do
            let env = emptyEnv
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS"] env
            env <- getMutPatt "thread-stepping" [VPMetaVar "TI"] env
            env <- getMutPatt "thread-map" [VPMetaVar "TM"] env
            env <- lifted_sideCondition (SCPatternMatch (TApp "lookup" [TVar "TM",TVar "TI"]) [PADT "thread" [PADT "thunk" [PADT "abstraction" [VPMetaVar "X"]]]]) env
            env <- lifted_sideCondition (SCPatternMatch (TVar "X") [VPAnnotated (VPMetaVar "V") (TName "values")]) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "map-delete" [TVar "TM",TSet [TVar "TI"]]) [VPMetaVar "TM'"]) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "set-difference" [TVar "ATS",TSet [TVar "TI"]]) [VPMetaVar "ATS'"]) env
            putMutTerm "active-thread-set" (TVar "ATS'") env
            putMutTerm "thread-stepping" (TSeq []) env
            putMutTerm "thread-map" (TVar "TM'") env
            stepTermTo (TName "null-value") env
          step3 = do
            let env = emptyEnv
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS"] env
            env <- getMutPatt "thread-stepping" [VPMetaVar "TI"] env
            env <- getMutPatt "thread-map" [VPMetaVar "TM"] env
            env <- getMutPatt "terminated-thread-map" [VPMetaVar "TVM"] env
            env <- lifted_sideCondition (SCPatternMatch (TApp "lookup" [TVar "TM",TVar "TI"]) [PADT "thread" [PADT "thunk" [PADT "abstraction" [VPMetaVar "X"]],PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),VPSeqVar "TI*" StarOp]]]) env
            env <- lifted_sideCondition (SCPatternMatch (TVar "X") [VPAnnotated (VPMetaVar "V") (TName "values")]) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "map-delete" [TVar "TM",TSet [TVar "TI"]]) [VPMetaVar "TM'"]) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "set-unite" [TApp "set-difference" [TVar "ATS",TSet [TVar "TI"]],TSet [TVar "TI*"]]) [VPMetaVar "ATS'"]) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "map-unite" [TVar "TVM",TMap [TBinding (TVar "TI") (TVar "V")]]) [VPMetaVar "TVM'"]) env
            putMutTerm "active-thread-set" (TVar "ATS'") env
            putMutTerm "thread-stepping" (TSeq []) env
            putMutTerm "thread-map" (TVar "TM'") env
            putMutTerm "terminated-thread-map" (TVar "TVM'") env
            stepTermTo (TName "null-value") env

thread_atomic_ fargs = FApp "thread-atomic" (fargs)
stepThread_atomic fargs =
    evalRules [] [step1,step2,step3,step4,step5,step6]
    where step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getMutPatt "store" [VPMetaVar "Sigma"] env
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS"] env
            env <- getMutPatt "thread-stepping" [VPMetaVar "TI"] env
            env <- getControlPatt "abrupted" (Nothing) env
            putMutTerm "store" (TVar "Sigma") env
            putMutTerm "active-thread-set" (TVar "ATS") env
            putMutTerm "thread-stepping" (TVar "TI") env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Nothing) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varabrupted (Nothing) env
            env <- getMutPatt "store" [VPMetaVar "Sigma'"] env
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS'"] env
            env <- getMutPatt "thread-stepping" [VPMetaVar "TI'"] env
            putMutTerm "store" (TVar "Sigma'") env
            putMutTerm "active-thread-set" (TVar "ATS'") env
            putMutTerm "thread-stepping" (TVar "TI'") env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Nothing) env (premise (TApp "thread-atomic" [TVar "X'"]) [PMetaVar "V"] env))
            env <- receiveSignalPatt __varabrupted (Nothing) env
            env <- getMutPatt "store" [VPMetaVar "Sigma''"] env
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS''"] env
            env <- getMutPatt "thread-stepping" [VPSeqVar "TI''?" QuestionMarkOp] env
            putMutTerm "store" (TVar "Sigma''") env
            putMutTerm "active-thread-set" (TVar "ATS''") env
            putMutTerm "thread-stepping" (TVar "TI''?") env
            stepTermTo (TVar "V") env
          step2 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getMutPatt "store" [VPMetaVar "Sigma"] env
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS"] env
            env <- getMutPatt "thread-stepping" [VPMetaVar "TI"] env
            env <- getControlPatt "abrupted" (Just (VPMetaVar "A")) env
            putMutTerm "store" (TVar "Sigma") env
            putMutTerm "active-thread-set" (TVar "ATS") env
            putMutTerm "thread-stepping" (TVar "TI") env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Nothing) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varabrupted (Nothing) env
            env <- getMutPatt "store" [VPMetaVar "Sigma'"] env
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS'"] env
            env <- getMutPatt "thread-stepping" [VPMetaVar "TI'"] env
            putMutTerm "store" (TVar "Sigma'") env
            putMutTerm "active-thread-set" (TVar "ATS'") env
            putMutTerm "thread-stepping" (TVar "TI'") env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Just (TVar "A")) env (premise (TApp "thread-atomic" [TVar "X'"]) [PMetaVar "V"] env))
            env <- receiveSignalPatt __varabrupted (Just (VPMetaVar "A")) env
            env <- getMutPatt "store" [VPMetaVar "Sigma''"] env
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS''"] env
            env <- getMutPatt "thread-stepping" [VPSeqVar "TI''?" QuestionMarkOp] env
            raiseTerm "abrupted" (TVar "A") env
            putMutTerm "store" (TVar "Sigma''") env
            putMutTerm "active-thread-set" (TVar "ATS''") env
            putMutTerm "thread-stepping" (TVar "TI''?") env
            stepTermTo (TVar "V") env
          step3 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getControlPatt "abrupted" (Just (VPMetaVar "A")) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Just (TVar "A")) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varabrupted (Just (VPMetaVar "A")) env
            raiseTerm "abrupted" (TVar "A") env
            stepTermTo (TApp "thread-atomic" [TVar "X'"]) env
          step4 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getControlPatt "abrupted" (Nothing) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Nothing) env (premise (TVar "X") [PAnnotated (PMetaVar "V") (TName "values")] env))
            env <- receiveSignalPatt __varabrupted (Nothing) env
            stepTermTo (TVar "V") env
          step5 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getControlPatt "abrupted" (Just (VPMetaVar "A")) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Just (TVar "A")) env (premise (TVar "X") [PAnnotated (PMetaVar "V") (TName "values")] env))
            env <- receiveSignalPatt __varabrupted (Just (VPMetaVar "A")) env
            raiseTerm "abrupted" (TVar "A") env
            stepTermTo (TApp "thread-atomic" [TVar "V"]) env
          step6 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PAnnotated (PMetaVar "V") (TName "values")] env
            stepTermTo (TVar "V") env

thread_yield_ fargs = FApp "thread-yield" (fargs)
stepThread_yield fargs =
    evalRules [] [step1,step2,step3]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [] env
            putMutTerm "thread-stepping" (TSeq []) env
            stepTermTo (TName "null-value") env
          step2 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "TI") (TName "thread-ids")] env
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS"] env
            env <- lifted_sideCondition (SCEquality (TApp "is-in-set" [TVar "TI",TVar "ATS"]) (TName "true")) env
            putMutTerm "thread-stepping" (TVar "TI") env
            stepTermTo (TName "null-value") env
          step3 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "TI") (TName "thread-ids")] env
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS"] env
            env <- lifted_sideCondition (SCEquality (TApp "is-in-set" [TVar "TI",TVar "ATS"]) (TName "false")) env
            stepTermTo (TName "fail") env

thread_spin_ fargs = FApp "thread-spin" (fargs)
stepThread_spin fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "X"] env
            rewriteTermTo (TApp "else" [TVar "X",TApp "sequential" [TApp "thread-yield" [],TApp "thread-spin" [TVar "X"]]]) env

thread_suspend_ fargs = FApp "thread-suspend" (fargs)
stepThread_suspend fargs =
    evalRules [] [step1,step2]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPSeqVar "TI+" PlusOp) (TSortSeq (TName "thread-ids") PlusOp)] env
            env <- getMutPatt "thread-stepping" [VPMetaVar "TI"] env
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS"] env
            env <- lifted_sideCondition (SCEquality (TApp "is-in-set" [TVar "TI",TSet [TVar "TI+"]]) (TName "false")) env
            env <- lifted_sideCondition (SCEquality (TApp "is-subset" [TSet [TVar "TI+"],TVar "ATS"]) (TName "true")) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "set-difference" [TVar "ATS",TSet [TVar "TI+"]]) [VPMetaVar "ATS'"]) env
            putMutTerm "thread-stepping" (TVar "TI") env
            putMutTerm "active-thread-set" (TVar "ATS'") env
            stepTermTo (TName "null-value") env
          step2 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPSeqVar "TI+" PlusOp) (TSortSeq (TName "thread-ids") PlusOp)] env
            env <- getMutPatt "thread-stepping" [VPMetaVar "TI"] env
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS"] env
            env <- lifted_sideCondition (SCEquality (TApp "is-in-set" [TVar "TI",TSet [TVar "TI+"]]) (TName "true")) env
            env <- lifted_sideCondition (SCEquality (TApp "is-subset" [TSet [TVar "TI+"],TVar "ATS"]) (TName "true")) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "set-difference" [TVar "ATS",TSet [TVar "TI+"]]) [VPMetaVar "ATS'"]) env
            putMutTerm "thread-stepping" (TSeq []) env
            putMutTerm "active-thread-set" (TVar "ATS'") env
            stepTermTo (TName "null-value") env

thread_resume_ fargs = FApp "thread-resume" (fargs)
stepThread_resume fargs =
    evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPSeqVar "TI*" StarOp) (TSortSeq (TName "thread-ids") StarOp)] env
            env <- getMutPatt "thread-stepping" [VPMetaVar "TI"] env
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS"] env
            env <- lifted_sideCondition (SCEquality (TApp "is-in-set" [TVar "TI",TSet [TVar "TI*"]]) (TName "false")) env
            env <- lifted_sideCondition (SCEquality (TApp "set-intersect" [TVar "ATS",TSet [TVar "TI*"]]) (TSet [])) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "set-unite" [TVar "ATS",TSet [TVar "TI*"]]) [VPMetaVar "ATS'"]) env
            putMutTerm "thread-stepping" (TVar "TI") env
            putMutTerm "active-thread-set" (TVar "ATS'") env
            stepTermTo (TName "null-value") env

thread_terminate_ fargs = FApp "thread-terminate" (fargs)
stepThread_terminate fargs =
    evalRules [] [step1,step2]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "TI'") (TName "thread-ids")] env
            env <- getMutPatt "thread-stepping" [VPMetaVar "TI"] env
            env <- getMutPatt "thread-map" [VPMetaVar "TM"] env
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS"] env
            env <- lifted_sideCondition (SCPatternMatch (TApp "lookup" [TVar "TM",TVar "TI'"]) [PADT "thread" [PADT "thunk" [PADT "abstraction" [VPMetaVar "X"]]]]) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "map-delete" [TVar "TM",TSet [TVar "TI'"]]) [VPMetaVar "TM'"]) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "set-difference" [TVar "ATS",TSet [TVar "TI'"]]) [VPMetaVar "ATS'"]) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "when-true" [TApp "not" [TApp "is-equal" [TVar "TI",TVar "TI'"]],TVar "TI"]) [VPSeqVar "TI?" QuestionMarkOp]) env
            putMutTerm "thread-stepping" (TVar "TI?") env
            putMutTerm "thread-map" (TVar "TM'") env
            putMutTerm "active-thread-set" (TVar "ATS'") env
            stepTermTo (TName "null-value") env
          step2 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "TI'") (TName "thread-ids"),VPAnnotated (VPMetaVar "V") (TName "values")] env
            env <- getMutPatt "thread-stepping" [VPMetaVar "TI"] env
            env <- getMutPatt "thread-map" [VPMetaVar "TM"] env
            env <- getMutPatt "terminated-thread-map" [VPMetaVar "TVM"] env
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS"] env
            env <- lifted_sideCondition (SCPatternMatch (TApp "lookup" [TVar "TM",TVar "TI'"]) [PADT "thread" [PADT "thunk" [PADT "abstraction" [VPMetaVar "X"]],PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),VPSeqVar "TI*" StarOp]]]) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "map-delete" [TVar "TM",TSet [TVar "TI'"]]) [VPMetaVar "TM'"]) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "set-unite" [TApp "set-difference" [TVar "ATS",TSet [TVar "TI'"]],TSet [TVar "TI*"]]) [VPMetaVar "ATS'"]) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "map-unite" [TVar "TVM",TMap [TBinding (TVar "TI'") (TVar "V")]]) [VPMetaVar "TVM'"]) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "when-true" [TApp "not" [TApp "is-equal" [TVar "TI",TVar "TI'"]],TVar "TI"]) [VPSeqVar "TI?" QuestionMarkOp]) env
            putMutTerm "thread-stepping" (TVar "TI?") env
            putMutTerm "thread-map" (TVar "TM'") env
            putMutTerm "terminated-thread-map" (TVar "TVM'") env
            putMutTerm "active-thread-set" (TVar "ATS'") env
            stepTermTo (TName "null-value") env

is_thread_terminated_ fargs = FApp "is-thread-terminated" (fargs)
stepIs_thread_terminated fargs =
    evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "TI") (TName "thread-ids")] env
            env <- getMutPatt "terminated-thread-map" [VPMetaVar "TVM"] env
            stepTermTo (TApp "is-value" [TApp "map-lookup" [TVar "TVM",TVar "TI"]]) env

thread_value_ fargs = FApp "thread-value" (fargs)
stepThread_value fargs =
    evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "TI") (TName "thread-ids")] env
            env <- getMutPatt "terminated-thread-map" [VPMetaVar "TVM"] env
            stepTermTo (TApp "checked" [TApp "map-lookup" [TVar "TVM",TVar "TI"]]) env

thread_join_ fargs = FApp "thread-join" (fargs)
stepThread_join fargs =
    evalRules [] [step1,step2,step3,step4]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "TI'") (TName "thread-ids")] env
            env <- getMutPatt "thread-map" [VPMetaVar "TM"] env
            env <- getMutPatt "thread-stepping" [VPMetaVar "TI"] env
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS"] env
            env <- lifted_sideCondition (SCPatternMatch (TApp "lookup" [TVar "TM",TVar "TI'"]) [PADT "thread" [VPMetaVar "TH",PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),VPSeqVar "TI*" StarOp]]]) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "map-override" [TMap [TBinding (TVar "TI'") (TApp "thread" [TVar "TH",TApp "list" [TVar "TI*",TVar "TI"]])],TVar "TM"]) [VPMetaVar "TM'"]) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "set-difference" [TVar "ATS",TSet [TVar "TI"]]) [VPMetaVar "ATS'"]) env
            putMutTerm "thread-map" (TVar "TM'") env
            putMutTerm "thread-stepping" (TSeq []) env
            putMutTerm "active-thread-set" (TVar "ATS'") env
            stepTermTo (TName "null-value") env
          step2 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "TI'") (TName "thread-ids")] env
            env <- getMutPatt "terminated-thread-map" [VPMetaVar "TVM"] env
            env <- lifted_sideCondition (SCEquality (TApp "is-value" [TApp "lookup" [TVar "TVM",TVar "TI'"]]) (TName "true")) env
            stepTermTo (TName "null-value") env
          step3 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "TI'") (TName "thread-ids")] env
            env <- getMutPatt "thread-map" [VPMetaVar "TM"] env
            env <- lifted_sideCondition (SCPatternMatch (TApp "lookup" [TVar "TM",TVar "TI'"]) [PADT "thread" [VPMetaVar "TH"]]) env
            stepTermTo (TName "fail") env
          step4 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "TI'") (TName "thread-ids")] env
            env <- getMutPatt "thread-map" [VPMetaVar "TM"] env
            env <- getMutPatt "terminated-thread-map" [VPMetaVar "TVM"] env
            env <- lifted_sideCondition (SCEquality (TApp "lookup" [TVar "TM",TVar "TI'"]) (TSeq [])) env
            env <- lifted_sideCondition (SCEquality (TApp "lookup" [TVar "TVM",TVar "TI'"]) (TSeq [])) env
            stepTermTo (TName "fail") env

thread_exterminate_ fargs = FApp "thread-exterminate" (fargs)
stepThread_exterminate fargs =
    evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "TI'") (TName "thread-ids")] env
            env <- getMutPatt "thread-map" [VPMetaVar "TM"] env
            env <- getMutPatt "thread-stepping" [VPMetaVar "TI"] env
            env <- getMutPatt "terminated-thread-map" [VPMetaVar "TVM"] env
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS"] env
            env <- lifted_sideCondition (SCInequality (TVar "TI'") (TVar "TI")) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "map-delete" [TVar "TM",TSet [TVar "TI'"]]) [VPMetaVar "TM'"]) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "map-delete" [TVar "TVM",TSet [TVar "TI'"]]) [VPMetaVar "TVM'"]) env
            env <- lifted_sideCondition (SCPatternMatch (TApp "set-difference" [TVar "ATS",TSet [TVar "TI'"]]) [VPMetaVar "ATS'"]) env
            putMutTerm "thread-map" (TVar "TM'") env
            putMutTerm "thread-stepping" (TVar "TI") env
            putMutTerm "terminated-thread-map" (TVar "TVM'") env
            putMutTerm "active-thread-set" (TVar "ATS'") env
            stepTermTo (TName "null-value") env

update_thread_stepping_ = FName "update-thread-stepping"
stepUpdate_thread_stepping = evalRules [] [step1,step2,step3,step4]
    where step1 = do
            let env = emptyEnv
            env <- getMutPatt "thread-stepping" [VPMetaVar "TI"] env
            env <- premise (TApp "is-thread-preemptible" [TVar "TI"]) [PValue (PADT "false" [])] env
            stepTermTo (TName "null-value") env
          step2 = do
            let env = emptyEnv
            env <- getMutPatt "thread-stepping" [VPMetaVar "TI"] env
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS"] env
            env <- premise (TApp "is-thread-preemptible" [TVar "TI"]) [PValue (PADT "true" [])] env
            env <- lifted_sideCondition (SCPatternMatch (TApp "some-element" [TVar "ATS"]) [VPMetaVar "TI'"]) env
            putMutTerm "thread-stepping" (TVar "TI'") env
            stepTermTo (TName "null-value") env
          step3 = do
            let env = emptyEnv
            env <- getMutPatt "thread-stepping" [] env
            env <- getMutPatt "active-thread-set" [VPMetaVar "ATS"] env
            env <- lifted_sideCondition (SCPatternMatch (TApp "some-element" [TVar "ATS"]) [VPMetaVar "TI'"]) env
            putMutTerm "thread-stepping" (TVar "TI'") env
            stepTermTo (TName "null-value") env
          step4 = do
            let env = emptyEnv
            env <- getMutPatt "thread-stepping" [VPSeqVar "TI?" QuestionMarkOp] env
            env <- getMutPatt "active-thread-set" [PADT "set" []] env
            putMutTerm "thread-stepping" (TSeq []) env
            stepTermTo (TName "null-value") env

update_thread_schedule_ fargs = FApp "update-thread-schedule" (fargs)
stepUpdate_thread_schedule fargs =
    evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- lifted_vsMatch fargs [VPAnnotated (VPMetaVar "VS") (TApp "sets" [TName "ground-values"])] env
            putMutTerm "thread-schedule" (TVar "VS") env
            stepTermTo (TName "null-value") env

current_thread_schedule_ = FName "current-thread-schedule"
stepCurrent_thread_schedule = evalRules [] [step1]
    where step1 = do
            let env = emptyEnv
            env <- getMutPatt "thread-schedule" [VPMetaVar "VS"] env
            stepTermTo (TVar "VS") env

thread_preemptible_ = FName "thread-preemptible"
stepThread_preemptible = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 104)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 100)]),FValue (ADTVal "unicode-character" [FValue (Int 45)]),FValue (ADTVal "unicode-character" [FValue (Int 112)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 109)]),FValue (ADTVal "unicode-character" [FValue (Int 112)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 98)]),FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 101)])]))]) env

thread_cooperative_ = FName "thread-cooperative"
stepThread_cooperative = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "datatype-value" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 104)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 100)]),FValue (ADTVal "unicode-character" [FValue (Int 45)]),FValue (ADTVal "unicode-character" [FValue (Int 99)]),FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 111)]),FValue (ADTVal "unicode-character" [FValue (Int 112)]),FValue (ADTVal "unicode-character" [FValue (Int 101)]),FValue (ADTVal "unicode-character" [FValue (Int 114)]),FValue (ADTVal "unicode-character" [FValue (Int 97)]),FValue (ADTVal "unicode-character" [FValue (Int 116)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 118)]),FValue (ADTVal "unicode-character" [FValue (Int 101)])]))]) env

is_thread_preemptible_ fargs = FApp "is-thread-preemptible" (fargs)
stepIs_thread_preemptible fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated VPWildCard (TName "thread-ids")] env
            rewriteTermTo (TApp "not" [TApp "is-in-set" [TName "thread-cooperative",TName "current-thread-schedule"]]) env

thread_ids_ = FName "thread-ids"
stepThread_ids = rewriteType "thread-ids" []

threads_ = FName "threads"
stepThreads = rewriteType "threads" []

thread_preemtibilities_ = FName "thread-preemtibilities"
stepThread_preemtibilities = rewriteType "thread-preemtibilities" []