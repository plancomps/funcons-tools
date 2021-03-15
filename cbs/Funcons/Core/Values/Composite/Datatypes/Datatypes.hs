-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Composite/Datatypes/Datatypes.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.Datatypes.Datatypes where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("datatype-value-id",StrictFuncon stepDatatype_value_id),("datatype-value-elements",StrictFuncon stepDatatype_value_elements)]

datatype_value_id_ fargs = FApp "datatype-value-id" (fargs)
stepDatatype_value_id fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPAnnotated (VPMetaVar "I") (TName "identifiers"),VPAnnotated (VPSeqVar "___" StarOp) (TSortSeq (TName "values") StarOp)]] env
            rewriteTermTo (TVar "I") env

datatype_value_elements_ fargs = FApp "datatype-value-elements" (fargs)
stepDatatype_value_elements fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPAnnotated VPWildCard (TName "identifiers"),VPAnnotated (VPSeqVar "V*" StarOp) (TSortSeq (TName "values") StarOp)]] env
            rewriteTermTo (TVar "V*") env