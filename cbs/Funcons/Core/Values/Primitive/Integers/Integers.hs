-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Primitive/Integers/Integers.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Primitive.Integers.Integers where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("bounded-integers",StrictFuncon stepBounded_integers),("bounded-ints",StrictFuncon stepBounded_integers),("positive-integers",NullaryFuncon stepPositive_integers),("pos-ints",NullaryFuncon stepPositive_integers),("negative-integers",NullaryFuncon stepNegative_integers),("neg-ints",NullaryFuncon stepNegative_integers),("natural-numbers",NullaryFuncon stepNatural_numbers),("nats",NullaryFuncon stepNatural_numbers),("integer-negate",StrictFuncon stepInteger_negate),("int-neg",StrictFuncon stepInteger_negate),("integer-sequence",StrictFuncon stepInteger_sequence)]

bounded_integers_ fargs = FApp "bounded-integers" (fargs)
bounded_ints_ fargs = FApp "bounded-integers" (fargs)
stepBounded_integers fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "M") (TName "integers"),VPAnnotated (VPMetaVar "N") (TName "integers")] env
            rewriteTermTo (TSortInter (TApp "integers-from" [TVar "M"]) (TApp "integers-up-to" [TVar "N"])) env

positive_integers_ = FName "positive-integers"
pos_ints_ = FName "positive-integers"
stepPositive_integers = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "integers-from" [TFuncon (FValue (Nat 1))]) env

negative_integers_ = FName "negative-integers"
neg_ints_ = FName "negative-integers"
stepNegative_integers = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "integers-up-to" [TFuncon (FValue (Nat (-1)))]) env

natural_numbers_ = FName "natural-numbers"
nats_ = FName "natural-numbers"
stepNatural_numbers = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "integers-from" [TFuncon (FValue (Nat 0))]) env

integer_negate_ fargs = FApp "integer-negate" (fargs)
int_neg_ fargs = FApp "integer-negate" (fargs)
stepInteger_negate fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "N") (TName "integers")] env
            rewriteTermTo (TApp "integer-subtract" [TFuncon (FValue (Nat 0)),TVar "N"]) env

integer_sequence_ fargs = FApp "integer-sequence" (fargs)
stepInteger_sequence fargs =
    evalRules [rewrite1,rewrite2] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "M") (TName "integers"),VPAnnotated (VPMetaVar "N") (TName "integers")] env
            env <- sideCondition (SCEquality (TApp "is-greater" [TVar "M",TVar "N"]) (TName "false")) env
            rewriteTermTo (TSeq [TVar "M",TApp "integer-sequence" [TApp "integer-add" [TVar "M",TFuncon (FValue (Nat 1))],TVar "N"]]) env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [VPAnnotated (VPMetaVar "M") (TName "integers"),VPAnnotated (VPMetaVar "N") (TName "integers")] env
            env <- sideCondition (SCEquality (TApp "is-greater" [TVar "M",TVar "N"]) (TName "true")) env
            rewriteTermTo (TSeq []) env