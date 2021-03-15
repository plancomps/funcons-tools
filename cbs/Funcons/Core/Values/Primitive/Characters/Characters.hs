-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Values/Primitive/Characters/Characters.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Primitive.Characters.Characters where

import Funcons.EDSL

import Funcons.Operations hiding (Values,libFromList)
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("unicode-point",StrictFuncon stepUnicode_point),("unicode",StrictFuncon stepUnicode_point),("iso-latin-1-points",NullaryFuncon stepIso_latin_1_points),("ascii-points",NullaryFuncon stepAscii_points),("ascii-character",StrictFuncon stepAscii_character),("ascii-char",StrictFuncon stepAscii_character),("backspace",NullaryFuncon stepBackspace),("horizontal-tab",NullaryFuncon stepHorizontal_tab),("line-feed",NullaryFuncon stepLine_feed),("form-feed",NullaryFuncon stepForm_feed),("carriage-return",NullaryFuncon stepCarriage_return),("double-quote",NullaryFuncon stepDouble_quote),("single-quote",NullaryFuncon stepSingle_quote),("backslash",NullaryFuncon stepBackslash)]

unicode_point_ fargs = FApp "unicode-point" (fargs)
unicode_ fargs = FApp "unicode-point" (fargs)
stepUnicode_point fargs =
    evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "unicode-character" [VPAnnotated (VPMetaVar "UP") (TName "unicode-points")]] env
            rewriteTermTo (TVar "UP") env

iso_latin_1_points_ = FName "iso-latin-1-points"
stepIso_latin_1_points = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "bounded-integers" [TFuncon (FValue (Nat 0)),TApp "unsigned-bit-vector-maximum" [TFuncon (FValue (Nat 8))]]) env

ascii_points_ = FName "ascii-points"
stepAscii_points = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "bounded-integers" [TFuncon (FValue (Nat 0)),TApp "unsigned-bit-vector-maximum" [TFuncon (FValue (Nat 7))]]) env

ascii_character_ fargs = FApp "ascii-character" (fargs)
ascii_char_ fargs = FApp "ascii-character" (fargs)
stepAscii_character fargs =
    evalRules [rewrite1,rewrite2,rewrite3] []
    where rewrite1 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),VPAnnotated (VPMetaVar "C") (TName "ascii-characters")]] env
            rewriteTermTo (TVar "C") env
          rewrite2 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),VPAnnotated (VPMetaVar "C") (TName "characters")]] env
            env <- sideCondition (SCIsInSort (TVar "C") (TSortComplement (TName "ascii-characters"))) env
            rewriteTermTo (TSeq []) env
          rewrite3 = do
            let env = emptyEnv
            env <- vsMatch fargs [PADT "datatype-value" [VPLit (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 108)]),FValue (ADTVal "unicode-character" [FValue (Int 105)]),FValue (ADTVal "unicode-character" [FValue (Int 115)]),FValue (ADTVal "unicode-character" [FValue (Int 116)])]),VPAnnotated (VPSeqVar "C*" StarOp) (TSortSeq (TName "characters") StarOp)]] env
            env <- sideCondition (SCInequality (TApp "length" [TVar "C*"]) (TFuncon (FValue (Nat 1)))) env
            rewriteTermTo (TSeq []) env

backspace_ = FName "backspace"
stepBackspace = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "unicode-character" [TApp "hexadecimal-natural" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 56)])]))]]) env

horizontal_tab_ = FName "horizontal-tab"
stepHorizontal_tab = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "unicode-character" [TApp "hexadecimal-natural" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 57)])]))]]) env

line_feed_ = FName "line-feed"
stepLine_feed = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "unicode-character" [TApp "hexadecimal-natural" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 97)])]))]]) env

form_feed_ = FName "form-feed"
stepForm_feed = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "unicode-character" [TApp "hexadecimal-natural" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 99)])]))]]) env

carriage_return_ = FName "carriage-return"
stepCarriage_return = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "unicode-character" [TApp "hexadecimal-natural" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 100)])]))]]) env

double_quote_ = FName "double-quote"
stepDouble_quote = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "unicode-character" [TApp "hexadecimal-natural" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 50)]),FValue (ADTVal "unicode-character" [FValue (Int 50)])]))]]) env

single_quote_ = FName "single-quote"
stepSingle_quote = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "unicode-character" [TApp "hexadecimal-natural" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 50)]),FValue (ADTVal "unicode-character" [FValue (Int 55)])]))]]) env

backslash_ = FName "backslash"
stepBackslash = evalRules [rewrite1] []
    where rewrite1 = do
            let env = emptyEnv
            rewriteTermTo (TApp "unicode-character" [TApp "hexadecimal-natural" [TFuncon (FValue (ADTVal "list" [FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 48)]),FValue (ADTVal "unicode-character" [FValue (Int 53)]),FValue (ADTVal "unicode-character" [FValue (Int 99)])]))]]) env