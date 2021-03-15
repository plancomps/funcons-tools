-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Composite/Strings/Strings.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.Strings.Strings where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("strings",NullaryFuncon stepStrings),("string",StrictFuncon stepString),("string-append",StrictFuncon stepString_append)]

strings_ = FName "strings"
stepStrings = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "lists" [TName "characters"]) env

string_ fargs = FApp "string" (fargs)
stepString fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPSeqVar "C*" StarOp) (TSortSeq (TName "characters") StarOp)] env
            rewriteTermTo (TApp "list" [TVar "C*"]) env

string_append_ fargs = FApp "string-append" (fargs)
stepString_append fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPSeqVar "S*" StarOp) (TSortSeq (TName "strings") StarOp)] env
            rewriteTermTo (TApp "list-append" [TVar "S*"]) env