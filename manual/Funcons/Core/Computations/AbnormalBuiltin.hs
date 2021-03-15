-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Computations/Abnormal/Throwing/Throwing.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.AbnormalBuiltin where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)

library = libFromList
    [("handle-thrown",NonStrictFuncon stepHandle_thrown)
    ,("handle-return",NonStrictFuncon stepHandle_return)
    ,("else", NonStrictFuncon stepElse)
    ]


handle_thrown_ fargs = FApp "handle-thrown" (fargs)
stepHandle_thrown fargs =
    evalRules [rewrite1] [step1]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "V") (TName "values"),PMetaVar "Y"] env
            rewriteTermTo (TVar "V") env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X",PMetaVar "Y"] env
            env <- getControlPatt "abrupted" (Nothing) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Nothing) env (premise (TVar "X") [PMetaVar "X'"] env))
            case __varabrupted of 
              Nothing -> stepTermTo (TApp "handle-thrown" [TVar "X'",TVar "Y"]) env
              Just (ADTVal "thrown" [v]) -> stepTermTo (TApp "give" [TFuncon v, TVar "Y"]) env
              Just v  -> do raiseSignal "abrupted" v 
                            stepTermTo (TApp "handle-thrown" [TVar "X'",TVar "Y"]) env

stepHandle_return fargs =
    evalRules [rewrite1] [step1]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "V") (TName "values")] env
            rewriteTermTo (TVar "V") env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X"] env
            env <- getControlPatt "abrupted" (Nothing) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Nothing) env (premise (TVar "X") [PMetaVar "X'"] env))
            case __varabrupted of 
              Nothing -> stepTermTo (TApp "handle-return" [TVar "X'"]) env
              Just (ADTVal "returned" [v]) -> stepTo v 
              Just v -> do  raiseSignal "abrupted" v
                            stepTermTo (TApp "handle-return" [TVar "X'"]) env

stepElse fargs =
    evalRules [rewrite1,rewrite2] [step1]
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PAnnotated (PMetaVar "V") (TName "values"),PMetaVar "Y"] env
            rewriteTermTo (TVar "V") env
          rewrite2 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "X",PMetaVar "Y",PSeqVar "Z+" PlusOp] env
            rewriteTermTo (TApp "else" [TVar "X",TApp "else" [TVar "Y",TVar "Z+"]]) env
          step1 = do
            let env = emptyEnv
            env <- lifted_fsMatch fargs [PMetaVar "X",PMetaVar "Y"] env
            env <- getControlPatt "abrupted" (Nothing) env
            (env,[__varabrupted]) <- receiveSignals ["abrupted"] (withControlTerm "abrupted" (Nothing) env (premise (TVar "X") [PMetaVar "X'"] env))
            case __varabrupted of 
              Nothing -> stepTermTo (TApp "else" [TVar "X'",TVar "Y"]) env
              Just (ADTVal "failed" _) -> stepTermTo (TVar "Y") env
              Just v -> do raiseSignal "abrupted" v
                           stepTermTo (TApp "else" [TVar "X'",TVar "Y"]) env


