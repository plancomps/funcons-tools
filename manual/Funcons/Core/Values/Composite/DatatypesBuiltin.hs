{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Funcons.Core.Values.Composite.DatatypesBuiltin where

import Funcons.EDSL
import Funcons.MSOS (evalStrictSequence, evalSequence, Strictness(..))
import Funcons.Operations (Values(..), Types(..), ComputationTypes(..))
import Data.Text (pack,unpack)

library = libFromList [
        ("datatype-value", NonStrictFuncon evalADT)
    ,   ("non-strict-datatype-value", NonStrictFuncon evalLazyADT)
    -- backwards compatibility
    ,   ("adt-construct", StrictFuncon adtConstruct)
    ,   ("adt-type-construct", StrictFuncon adtTypeConstruct)
    ,   ("adt-constructor", StrictFuncon adtConstructor)
    ,   ("adt-fields", StrictFuncon adtFields)
    ]

datatype_value_ = applyFuncon "datatype-value"
adt_val_ = datatype_value_

evalADT (f:fs) = evalStrictSequence (f:fs) cont adt_val_
  where
    cont [] = error "eval-adt assert"
    cont (v:vs) = if isString_ v 
      then rewritten $ ADTVal (pack (unString v)) $ map FValue vs 
      else sortErr (adt_val_ (map FValue (v:vs))) ("first argument of datatype-value not a string") 
evalADT [] = sortErr (adt_val_ [])
                    "algebraic-datatype not applied to a string and a sequence of fields"

lazy_adt_val_ = applyFuncon "non-strict-datatype-value"
evalLazyADT (f:fs) = evalSequence (Strict : replicate (length fs) NonStrict) 
                          (f:fs) cont lazy_adt_val_
  where
    cont [] = error "lazy-eval-adt assert"
    cont (v:vs) = case v of
      FValue s | isString_ s  
        -> rewritten $ ADTVal (pack (unString s)) vs
      _ -> sortErr (lazy_adt_val_ (v:vs)) ("first argument of value constructor not a string") 
evalLazyADT [] = sortErr (lazy_adt_val_ [])
                    "value constructor not applied to a string and a sequence of computations"

adt_construct_ = applyFuncon "adt-construct"
adtConstruct (v:vs) = 
  if isString_ v 
    then rewritten $ ADTVal (pack (unString v)) $ map FValue vs 
    else sortErr (adt_val_ (map FValue (v:vs))) ("first argument of adt-construct not a string") 
adtConstruct [] = sortErr (adt_val_ [])
                    "adt-construct not applied to a string and a sequence of fields"

adt_type_construct_ = applyFuncon "adt-type-construct"
adtTypeConstruct (v:vs) 
  | isString_ v = rewritten $ ComputationType $ Type $ ADT (pack (unString v)) fs
  | otherwise   = sortErr (adt_val_ (FValue v : fs)) ("first argument of adt-type-construct not a string") 
 where fs = map FValue vs
adtTypeConstruct [] = sortErr (adt_val_ [])
                    "adt-type-construct not applied to a string and a sequence of type arguments (values)"

adt_constructor_ = applyFuncon "adt-constructor"
adtConstructor [ADTVal cons _] = rewritten $ string__ (unpack cons)
adtConstructor vs = sortErr (adt_constructor_ (map FValue vs)) "argument of adt-constructor not an adt value"

adt_fields_ = applyFuncon "adt-fields"
adtFields [ADTVal _ fs] = rewritten $ ADTVal "list" fs
adtFields vs = sortErr (adt_fields_ (map FValue vs)) "argument of adt-fields not an adt value"
