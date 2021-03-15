{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Primitive.IntegersBuiltin where

import Funcons.EDSL hiding (integers_)
import qualified Funcons.Operations as VAL

import Funcons.Core.Values.Primitive.BoolBuiltin

library = libFromList [
    ("integer-add", fromValOp integer_add_ VAL.integer_add_)
  , ("int-add", fromValOp integer_add_ VAL.integer_add_)
  , ("int-mul", fromValOp integer_multiply_ VAL.integer_multiply_)
  , ("integer-multiply", fromValOp integer_multiply_ VAL.integer_multiply_)
  , ("integer-divide", fromValOp integer_divide_ VAL.integer_divide_)
  , ("int-div", fromValOp integer_divide_ VAL.integer_divide_)
  , ("integer-subtract", fromValOp integer_subtract_ VAL.integer_subtract_)
  , ("integer-sub", fromValOp integer_subtract_ VAL.integer_subtract_)
  , ("integer-power", fromValOp integer_power_ VAL.integer_power_)
  , ("int-pow", fromValOp integer_power_ VAL.integer_power_)
  , ("integer-list", fromValOp integer_list_ VAL.integer_list_)
  , ("integer-modulo", fromValOp integer_modulo_ VAL.integer_modulo_)
  , ("int-mod", fromValOp integer_modulo_ VAL.integer_modulo_)
  , ("integer-absolute-value", fromValOp integer_absolute_value_ VAL.integer_absolute_value_)
  , ("integer-abs", fromValOp integer_absolute_value_ VAL.integer_absolute_value_)
  , ("decimal-natural", fromValOp decimal_natural_ VAL.decimal_natural_)
  , ("decimal", fromValOp decimal_natural_ VAL.decimal_natural_)
  , ("hexadecimal-natural", fromValOp hexadecimal_natural_ VAL.hexadecimal_natural_)
  , ("hexadecimal", fromValOp hexadecimal_natural_ VAL.hexadecimal_natural_)
  , ("octal-natural", fromValOp octal_natural_ VAL.octal_natural_)
  , ("octal", fromValOp octal_natural_ VAL.octal_natural_)
  , ("binary-natural", fromValOp binary_natural_ VAL.binary_natural_)
  , ("binary", fromValOp binary_natural_ VAL.binary_natural_)
  , ("natural-predecessor", fromValOp natural_predecessor_ VAL.natural_predecessor_)
  , ("nat-pred", fromValOp natural_predecessor_ VAL.natural_predecessor_)
  , ("natural-successor", fromValOp natural_successor_ VAL.natural_successor_)
  , ("nat-succ", fromValOp natural_successor_ VAL.natural_successor_)
  , ("integer-is-less", fromValOp is_less_ VAL.is_less_)
  , ("is-less", fromValOp is_less_ VAL.is_less_)
  , ("integer-is-greater", fromValOp is_greater_ VAL.is_greater_)
  , ("is-greater", fromValOp is_greater_ VAL.is_greater_)
  , ("integer-is-greater-or-equal", fromValOp is_greater_or_equal_ VAL.is_greater_or_equal_)
  , ("is-greater-or-equal", fromValOp is_greater_or_equal_ VAL.is_greater_or_equal_)
  , ("integer-is-less-or-equal", fromValOp is_less_or_equal_ VAL.is_less_or_equal_)
  , ("is-less-or-equal", fromValOp is_less_or_equal_ VAL.is_less_or_equal_)
  , ("integers", fromNullaryValOp integers_ VAL.integers_)
  , ("ints", fromNullaryValOp integers_ VAL.integers_)
  , ("integers-from", fromValOp integers_from_ VAL.integers_from_)
  , ("from", fromValOp integers_from_ VAL.integers_from_)
  , ("integers-up-to", fromValOp integers_up_to_ VAL.integers_up_to_)
  , ("up-to", fromValOp integers_up_to_ VAL.integers_up_to_)
  ]

ints_ = integers_
integers_ = applyFuncon "integers"
integers_from_ = applyFuncon "integers-from"
from_ = applyFuncon "from"
integers_up_to_ = applyFuncon "integers-up-to"
up_to_ = applyFuncon "up-to"
natural_predecessor_, nat_pred_ :: [Funcons] -> Funcons
natural_predecessor_ = applyFuncon "natural-predecessor"
nat_pred_ = applyFuncon "nat-pred"
natural_successor_, nat_succ_ :: [Funcons] -> Funcons
natural_successor_ = applyFuncon "natural-successor"
nat_succ_ = applyFuncon "nat-succ"
int_add_ = FApp "integer-add"
integer_add_ = FApp "integer-add"
integer_multiply_ = FApp "integer-multiply" 
integer_divide_ = FApp "integer-divide"
integer_subtract_ = applyFuncon "integer-subtract"
integer_power_ = applyFuncon "integer-power"
integer_power_op vx = sortErr (applyFuncon "integer-power" (fvalues vx))
                            "integer-power not applied to two integers"
integer_list_ = applyFuncon "integer-list"
integer_modulo_ = applyFuncon "integer-modulo"
int_mod_ = applyFuncon "int-mod"
integer_mod_ = applyFuncon "integer-modulo"
integer_absolute_value_ = applyFuncon "integer-absolute-value"
decimal_natural_ = FApp "decimal-natural"
decimal_ = FApp "decimal"
octal_natural_ = FApp "octal-natural"
octal_ = FApp "octal"
binary_natural_ = FApp "binary-natural"
binary_ = FApp "binary"
hexadecimal_natural_ = FApp "hexadecimal-natural"
hexadecimal_ = FApp "hexadecimal"
is_less_ = applyFuncon "is-less" 
integer_is_less_ = applyFuncon "is-less" 
is_less_or_equal_ = FApp "is-less-or-equal"
integer_is_less_or_equal_ = FApp "is-less-or-equal"
is_greater_ = FApp "is-greater"
integer_is_greater_ = FApp "is-greater"
is_greater_or_equal_ = FApp "is-greater-or-equal"
integer_is_greater_or_equal_ = FApp "is-greater-or-equal"
