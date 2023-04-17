-- GeNeRaTeD fOr: /home/thomas/repos/plancomps/CBS-beta/Unstable-Funcons-beta/Computations/Abnormal/Postponing/Postponing.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Abnormal.Postponing.Postponing where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("postpone",NonStrictFuncon stepPostpone),("postpone-after-effect",NonStrictFuncon stepPostpone_after_effect),("after-effect",NonStrictFuncon stepAfter_effect)]

postpone_ fargs = FApp "postpone" (fargs)
stepPostpone fargs =
    evalRules [] [step1,step2]
    where step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getInhPatt "given-value" [VPMetaVar "V"] env
            env <- getControlPatt "postponing" (Just (VPMetaVar "A")) env
            (env,[__varpostponing]) <- receiveSignals ["postponing"] (withControlTerm "postponing" (Nothing) env (withInhTerm "given-value" (TVar "V") env (premise (TApp "closure" [TApp "give" [TVar "V",TVar "X"]]) [PMetaVar "A"] env)))
            env <- receiveSignalPatt __varpostponing (Nothing) env
            raiseTerm "postponing" (TVar "A") env
            stepTermTo (TName "null-value") env
          step2 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getInhPatt "given-value" [] env
            env <- getControlPatt "postponing" (Just (VPMetaVar "A")) env
            (env,[__varpostponing]) <- receiveSignals ["postponing"] (withControlTerm "postponing" (Nothing) env (withInhTerm "given-value" (TSeq []) env (premise (TApp "closure" [TApp "no-given" [TVar "X"]]) [PMetaVar "A"] env)))
            env <- receiveSignalPatt __varpostponing (Nothing) env
            raiseTerm "postponing" (TVar "A") env
            stepTermTo (TName "null-value") env

postpone_after_effect_ fargs = FApp "postpone-after-effect" (fargs)
stepPostpone_after_effect fargs =
    evalRules [rewrite1] [step1,step2]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "V") (TName "values")] env
            rewriteTermTo (TVar "V") env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getControlPatt "postponing" (Nothing) env
            (env,[__varpostponing]) <- receiveSignals ["postponing"] (withControlTerm "postponing" (Nothing) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varpostponing (Nothing) env
            stepTermTo (TApp "postpone-after-effect" [TVar "X'"]) env
          step2 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getControlPatt "postponing" (Nothing) env
            (env,[__varpostponing]) <- receiveSignals ["postponing"] (withControlTerm "postponing" (Just (TVar "A")) env (premise (TVar "X") [PMetaVar "X'"] env))
            env <- receiveSignalPatt __varpostponing (Just (VPMetaVar "A")) env
            env <- lifted_sideCondition (SCPatternMatch (TVar "A") [PADT "abstraction" [VPMetaVar "Y"]]) env
            stepTermTo (TApp "postpone-after-effect" [TApp "after-effect" [TVar "X'",TVar "Y"]]) env

after_effect_ fargs = FApp "after-effect" (fargs)
stepAfter_effect fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "X",PMetaVar "Y"] env
            rewriteTermTo (TApp "give" [TVar "X",TApp "sequential" [TVar "Y",TName "given"]]) env