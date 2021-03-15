-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Composite/Graphs/Graphs.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.Graphs.Graphs where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("directed-graphs",NonStrictFuncon stepDirected_graphs)]

directed_graphs_ fargs = FApp "directed-graphs" (fargs)
stepDirected_graphs fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- fsMatch fargs [PMetaVar "GT"] env
            rewriteTermTo (TApp "maps" [TVar "GT",TApp "sets" [TVar "GT"]]) env