{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Primitive.BitsBuiltin where

import Funcons.EDSL
import qualified Funcons.Operations as VAL

library = libFromList [
        ("bit-vector-not", fromValOp bit_vector_not_ VAL.bit_vector_not_)
    ,   ("bit-vector-and", fromValOp bit_vector_and_ VAL.bit_vector_and_)
    ,   ("bit-vector-or", fromValOp bit_vector_or_ VAL.bit_vector_or_)
    ,   ("bit-vector-xor", fromValOp bit_vector_xor_ VAL.bit_vector_xor_)
    ,   ("bit-vector-shift-left", fromValOp bit_vector_shift_left_ VAL.bit_vector_shift_left_)
    ,   ("bit-vector-logical-shift-right", fromValOp bit_vector_logical_shift_right_ VAL.bit_vector_logical_shift_right_)
    ,   ("bit-vector-arithmetic-shift-right", fromValOp bit_vector_arithmetical_shift_right_ VAL.bit_vector_arithmetical_shift_right_)
    ,   ("integer-to-bit-vector", fromValOp integer_to_bit_vector_ VAL.integer_to_bit_vector_)
    ,   ("bit-vector-to-integer", fromValOp bit_vector_to_integer_ VAL.bit_vector_to_integer_)
    ,   ("bit-vector-to-natural", fromValOp bit_vector_to_natural_ VAL.bit_vector_to_natural_)
    ]

bit_vector_not_ = applyFuncon "bit-vector-not"
bit_vector_and_ = applyFuncon "bit-vector-and"
bit_vector_or_ = applyFuncon "bit-vector-or"
bit_vector_xor_ = applyFuncon "bit-vector-xor"
bit_vector_shift_left_ = applyFuncon "bit-vector-shift-left"
bit_vector_logical_shift_right_ = applyFuncon "bit-vector-logical-shift-right"
bit_vector_arithmetical_shift_right_ = applyFuncon "bit-vector-arithmetical-shift-right"
integer_to_bit_vector_ = applyFuncon "integer-to-bit-vector"
bit_vector_to_integer_ = applyFuncon "bit-vector-to-integer"
bit_vector_to_natural_ = applyFuncon "bit-vector-to-natural"
