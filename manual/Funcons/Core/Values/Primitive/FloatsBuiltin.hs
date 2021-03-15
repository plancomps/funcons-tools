{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Primitive.FloatsBuiltin where

import Funcons.EDSL
import Funcons.Operations (Values(..), ComputationTypes(..), Types(..), isIEEEFormat, doubleFromIEEEFormat, tobool)
import Funcons.Core.Values.Primitive.BoolBuiltin
import Funcons.Core.Values.Primitive.IntegersBuiltin

import Data.Fixed (mod')

library = libFromList [
        ("ieee-float-float-power", ValueOp stepIEEE_Float_Power)
    ,   ("ieee-float-add", ValueOp stepIEEE_Float_Add)
    ,   ("ieee-float-truncate", ValueOp stepIEEE_Float_Truncate)
    ,   ("ieee-float-multiply", ValueOp stepIEEE_Float_Multiply)
    ,   ("ieee-float-subtract", ValueOp stepIEEE_Float_Subtract)
    ,   ("ieee-float-negate", ValueOp stepIEEE_Float_Negate)
    ,   ("ieee-float-divide", ValueOp stepIEEE_Float_Divide)
--    ,   ("signed-bits-maximum", ValueOp stepSigned_Bits_Maximum)
--    ,   ("signed-bits-minimum", ValueOp stepSigned_Bits_Minimum)
    ,   ("ieee-float-acos", ValueOp stepIEEE_Float_Acos)
    ,   ("ieee-float-asin", ValueOp stepIEEE_Float_Asin)
    ,   ("ieee-float-atan", ValueOp stepIEEE_Float_Atan)
    ,   ("ieee-float-atan2", ValueOp stepIEEE_Float_Atan2)
    ,   ("ieee-float-cos", ValueOp stepIEEE_Float_Cos)
    ,   ("ieee-float-cosh", ValueOp stepIEEE_Float_Cosh)
    ,   ("ieee-float-exp", ValueOp stepIEEE_Float_Exp)
    ,   ("ieee-float-log", ValueOp stepIEEE_Float_Log)
    ,   ("ieee-float-log10", ValueOp stepIEEE_Float_Log10)
    ,   ("ieee-float-sin", ValueOp stepIEEE_Float_Sin)
    ,   ("ieee-float-sinh", ValueOp stepIEEE_Float_Sinh)
    ,   ("ieee-float-sqrt", ValueOp stepIEEE_Float_Sqrt)
    ,   ("ieee-float-tan", ValueOp stepIEEE_Float_Tan)
    ,   ("ieee-float-tanh", ValueOp stepIEEE_Float_Tanh)
    ,   ("ieee-float-ceiling", ValueOp stepIEEE_Float_Ceiling)
    ,   ("ieee-float-floor", ValueOp stepIEEE_Float_Floor)
    ,   ("ieee-float-absolute-value", ValueOp stepIEEE_Float_Absolute_Value)
    ,   ("ieee-float-remainder", ValueOp stepIEEE_Float_Remainder)
    ,   ("ieee-float-is-less", ValueOp stepIEEE_Float_Is_Less)
    ,   ("ieee-float-is-less-or-equal", ValueOp stepIEEE_Float_Is_Less_Or_Equal)
    ,   ("ieee-float-is-greater-or-equal", ValueOp stepIEEE_Float_Is_Greater_Or_Equal)
    ,   ("ieee-float-is-greater", ValueOp stepIEEE_Float_Is_Greater) 
    ]

ieee_float_truncate = applyFuncon "ieee-float-truncate"
stepIEEE_Float_Truncate [IEEE_Float_64 f, ADTVal "binary64" _] = rewriteTo $ int_ (truncate f)
stepIEEE_Float_Truncate vn = sortErr (ieee_float_truncate (fvalues vn)) "ieee-float-truncate not applied to a float of the right format"
stepIEEE_Float_Add vs       = ieee_float_add_op vs
stepIEEE_Float_Multiply vs  = ieee_float_multiply_op vs
stepIEEE_Float_Subtract [f,f1,f2] = ieee_float_subtract_op f f1 f2
stepIEEE_Float_Subtract vn = sortErr (ieee_float_subtract (fvalues vn)) "sort check"
stepIEEE_Float_Negate [f,f1] = ieee_float_negate_op f f1
stepIEEE_Float_Negate vn = sortErr (ieee_float_negate (fvalues vn)) "sort check"
stepIEEE_Float_Divide [f,f1,f2] = ieee_float_divide_op f f1 f2
stepIEEE_Float_Divide vn = sortErr (ieee_float_divide (fvalues vn)) "sort check"
stepIEEE_Float_Power [f,f1,f2] = ieee_float_power_op f f1 f2
stepIEEE_Float_Power vn = sortErr (ieee_float_float_power (fvalues vn)) "sort check"

signed_bits_maximum = applyFuncon "signed-bits-maximum"
stepSigned_Bits_Maximum [vn] | Nat n <- upcastNaturals vn
        = rewriteTo $ integer_subtract_ [integer_power_ [int_ 2, integer_subtract_ [int_ $ fromInteger n, int_ 1]],int_ 1]
stepSigned_Bits_Maximum vs = sortErr (signed_bits_maximum (fvalues vs)) "sort check"

signed_bits_minimum = applyFuncon "signed-bits-minimum"
stepSigned_Bits_Minimum [vn] | Nat n <- upcastNaturals vn
        = rewriteTo $ applyFuncon "integer-negate" [signed_bits_maximum [FValue vn]]
stepSigned_Bits_Minimum vs = sortErr (signed_bits_maximum (fvalues vs)) "sort check"

    -- TODO binary64 assumption (perhaps use config files)
ieee_float_acos = applyFuncon "ieee-float-acos"
stepIEEE_Float_Acos [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (acos f1)
stepIEEE_Float_Acos vn = sortErr (ieee_float_acos (fvalues vn)) "sort check"

ieee_float_asin = applyFuncon "ieee-float-asin"
stepIEEE_Float_Asin [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (asin f1)
stepIEEE_Float_Asin vn = sortErr (ieee_float_asin (fvalues vn)) "sort check"

ieee_float_atan = applyFuncon "ieee-float-atan"
stepIEEE_Float_Atan [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (atan f1)
stepIEEE_Float_Atan vn = sortErr (ieee_float_atan (fvalues vn)) "sort check"

ieee_float_atan2 = applyFuncon "ieee-float-atan2"
stepIEEE_Float_Atan2 [f,vx,vy] = let f1 = doubleFromIEEEFormat f vx
                                     f2 = doubleFromIEEEFormat f vy
                                 in rewriteTo $ FValue $ IEEE_Float_64 (atan2 f1 f2)
stepIEEE_Float_Atan2 vn = sortErr (ieee_float_atan2 (fvalues vn)) "sort check"

ieee_float_cos = applyFuncon "ieee-float-cos"
stepIEEE_Float_Cos [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (cos f1)
stepIEEE_Float_Cos vn = sortErr (ieee_float_cos (fvalues vn)) "sort check"

ieee_float_cosh = applyFuncon "ieee-float-cosh"
stepIEEE_Float_Cosh [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (cosh f1)
stepIEEE_Float_Cosh vn = sortErr (ieee_float_cosh (fvalues vn)) "sort check"

ieee_float_exp = applyFuncon "ieee-float-exp"
stepIEEE_Float_Exp [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (exp f1)
stepIEEE_Float_Exp vn = sortErr (ieee_float_exp (fvalues vn)) "sort check"

ieee_float_log = applyFuncon "ieee-float-log"
stepIEEE_Float_Log [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (log f1)
stepIEEE_Float_Log vn = sortErr (ieee_float_log (fvalues vn)) "sort check"

ieee_float_log10 = applyFuncon "ieee-float-log10"
stepIEEE_Float_Log10 [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (logBase 10 f1)
stepIEEE_Float_Log10 vn = sortErr (ieee_float_log10 (fvalues vn)) "sort check"

ieee_float_sin = applyFuncon "ieee-float-sin"
stepIEEE_Float_Sin [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (sin f1)
stepIEEE_Float_Sin vn = sortErr (ieee_float_sin (fvalues vn)) "sort check"

ieee_float_sinh = applyFuncon "ieee-float-sinh"
stepIEEE_Float_Sinh [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (sinh f1)
stepIEEE_Float_Sinh vn = sortErr (ieee_float_sinh (fvalues vn)) "sort check"

ieee_float_sqrt = applyFuncon "ieee-float-sqrt"
stepIEEE_Float_Sqrt [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (sqrt f1)
stepIEEE_Float_Sqrt vn = sortErr (ieee_float_sqrt (fvalues vn)) "sort check"

ieee_float_tan = applyFuncon "ieee-float-tan"
stepIEEE_Float_Tan [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (tan f1)
stepIEEE_Float_Tan vn = sortErr (ieee_float_tan (fvalues vn)) "sort check"

ieee_float_tanh = applyFuncon "ieee-float-tanh"
stepIEEE_Float_Tanh [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (tanh f1)
stepIEEE_Float_Tanh vn = sortErr (ieee_float_tanh (fvalues vn)) "sort check"

ieee_float_ceiling = applyFuncon "ieee-float-ceiling"
stepIEEE_Float_Ceiling [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ int_ (ceiling f1)
stepIEEE_Float_Ceiling vn = sortErr (ieee_float_ceiling (fvalues vn)) "sort check"

ieee_float_floor = applyFuncon "ieee-float-floor"
stepIEEE_Float_Floor [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ int_ (floor f1)
stepIEEE_Float_Floor vn = sortErr (ieee_float_floor (fvalues vn)) "sort check"

ieee_float_absolute_value = applyFuncon "ieee-float-absolute-value"
stepIEEE_Float_Absolute_Value [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (Prelude.abs f1)
stepIEEE_Float_Absolute_Value vn = sortErr (ieee_float_absolute_value (fvalues vn)) "sort check"

stepIEEE_Float_Remainder [f,f1,f2] = ieee_float_remainder_op f f1 f2
stepIEEE_Float_Remainder vn = sortErr (ieee_float_remainder (fvalues vn)) "sort check"

stepIEEE_Float_Is_Less [f,f1,f2] = ieee_float_is_less_op f f1 f2
stepIEEE_Float_Is_Less vn = sortErr (ieee_float_is_less (fvalues vn)) "sort check"
stepIEEE_Float_Is_Greater [f,f1,f2] = ieee_float_is_greater_op f f1 f2
stepIEEE_Float_Is_Greater vn = sortErr (ieee_float_is_greater (fvalues vn)) "sort check"
stepIEEE_Float_Is_Less_Or_Equal [f,f1,f2] = ieee_float_is_less_or_equal_op f f1 f2
stepIEEE_Float_Is_Less_Or_Equal vn = sortErr (ieee_float_is_less_or_equal (fvalues vn)) "sort check"
stepIEEE_Float_Is_Greater_Or_Equal [f,f1,f2] = ieee_float_is_greater_or_equal_op f f1 f2
stepIEEE_Float_Is_Greater_Or_Equal vn = sortErr (ieee_float_is_greater_or_equal (fvalues vn)) "sort check"


ieee_float_op :: String -> ([Funcons] -> Funcons)
               -> (Double -> Double -> Double) 
                -> Double -> Funcons.EDSL.Values -> [Funcons.EDSL.Values] -> Rewrite Rewritten
ieee_float_op str cons f b format vs
    | all (isIEEEFormat format) vs = rewriteTo $ FValue $ IEEE_Float_64
        $ foldr f b $ map (doubleFromIEEEFormat format) vs
    | otherwise = sortErr (cons (map FValue vs)) err
    where   err     = str ++ " not applied to ieee_floats"

ieee_float_add = applyFuncon "ieee-float-add"
ieee_float_add_op (format:vs) = ieee_float_op "ieee_float-add" ieee_float_add (+) 0 format vs
ieee_float_add_op [] = sortErr (ieee_float_add [FValue (ADTVal "list" [])]) "ieee-float-add not applied to a format and a list of floats"

ieee_float_multiply_ = ieee_float_multiply 
ieee_float_multiply = applyFuncon "ieee-float-multiply"
ieee_float_multiply_op (format:vs) = ieee_float_op "ieee_float-multiply" ieee_float_multiply (*) 1 format vs
ieee_float_multiply_op [] = sortErr (ieee_float_multiply [FValue (ADTVal "list" [])]) "ieee-float-multiply not applied to a format and a list of floats"

ieee_float_divide = applyFuncon "ieee-float-divide"
ieee_float_divide_op format vx vy
    | isIEEEFormat format vx && isIEEEFormat format vy =
        let f1 = doubleFromIEEEFormat format vx
            f2 = doubleFromIEEEFormat format vy
        in rewriteTo $ FValue $ IEEE_Float_64 $ (f1 / f2)
ieee_float_divide_op ft vx vy = sortErr (ieee_float_divide [FValue ft,FValue vx, FValue vy])
                         "ieee-float-divide not applied to a format and ieee-floats"

ieee_float_remainder = applyFuncon "ieee-float-remainder"
ieee_float_remainder_op format vx vy
    | isIEEEFormat format vx =
        let f1 = doubleFromIEEEFormat format vx
            f2 = doubleFromIEEEFormat format vy
        in rewriteTo $ FValue $ IEEE_Float_64 $ (f1 `mod'` f2)
ieee_float_remainder_op ft vx vy = sortErr (ieee_float_remainder [FValue ft,FValue vx, FValue vy])
                         "ieee-float-remainder not applied to a format and ieee-floats"

ieee_float_negate = applyFuncon "ieee-float-negate"
ieee_float_negate_op format vx
    | isIEEEFormat format vx = let f1 = doubleFromIEEEFormat format vx
                               in rewriteTo $ FValue $ IEEE_Float_64 (-f1)
    | otherwise = sortErr (ieee_float_negate [FValue format,FValue vx]) "ieee-float-negate not applied to ieee-float"

ieee_float_subtract = applyFuncon "ieee-float-subtract"
ieee_float_subtract_op format vx vy
    | isIEEEFormat format vx && isIEEEFormat format vy =
        let f1 = doubleFromIEEEFormat format vx
            f2 = doubleFromIEEEFormat format vy
        in rewriteTo $ FValue $ IEEE_Float_64 $ (f1 - f2)
ieee_float_subtract_op ft vx vy = sortErr (ieee_float_subtract [FValue ft, FValue vx, FValue vy])
                         "ieee-float-subtract not applied to a format and ieee-floats"

ieee_float_float_power = applyFuncon "ieee-float-float-power"
ieee_float_power_op format vx vy
    | isIEEEFormat format vx && isIEEEFormat format vy =
        let f1 = doubleFromIEEEFormat format vx
            f2 = doubleFromIEEEFormat format vy
        in rewriteTo $ FValue $ IEEE_Float_64 $ (f1 ** f2)
ieee_float_power_op ft vx vy = sortErr (ieee_float_float_power [FValue ft, FValue vx, FValue vy])
                         "ieee-float-power not applied to a format and ieee-floats"

ieee_float_is_less = applyFuncon "ieee-float-is-less"
ieee_float_is_less_op format vx vy
    | isIEEEFormat format vx && isIEEEFormat format vy =
        let f1 = doubleFromIEEEFormat format vx
            f2 = doubleFromIEEEFormat format vy
        in rewriteTo $ FValue $ tobool (f1 < f2)
ieee_float_is_less_op ft vx vy = sortErr (ieee_float_is_less [FValue ft, FValue vx, FValue vy])
                         "ieee-float-is-less not applied to a format and ieee-floats"

ieee_float_is_greater = applyFuncon "ieee-float-is-greater"
ieee_float_is_greater_op format vx vy
    | isIEEEFormat format vx && isIEEEFormat format vy =
        let f1 = doubleFromIEEEFormat format vx
            f2 = doubleFromIEEEFormat format vy
        in rewriteTo $ FValue $ tobool (f1 > f2)
ieee_float_is_greater_op ft vx vy = sortErr (ieee_float_is_greater [FValue ft, FValue vx, FValue vy])
                         "ieee-float-is-greater not applied to a format and ieee-floats"

ieee_float_is_less_or_equal = applyFuncon "ieee-float-is-less-or-equal"
ieee_float_is_less_or_equal_op format vx vy
    | isIEEEFormat format vx && isIEEEFormat format vy =
        let f1 = doubleFromIEEEFormat format vx
            f2 = doubleFromIEEEFormat format vy
        in rewriteTo $ FValue $ tobool (f1 <= f2)
ieee_float_is_less_or_equal_op ft vx vy = sortErr (ieee_float_is_less_or_equal [FValue ft,FValue vx, FValue vy])
                         "ieee-float-is-less-or-equal not applied to a format and ieee-floats"

ieee_float_is_greater_or_equal = applyFuncon "ieee-float-is-greater-or-equal"
ieee_float_is_greater_or_equal_op format vx vy
    | isIEEEFormat format vx && isIEEEFormat format vy =
        let f1 = doubleFromIEEEFormat format vx
            f2 = doubleFromIEEEFormat format vy
        in rewriteTo $ FValue $ tobool (f1 >= f2)
ieee_float_is_greater_or_equal_op ft vx vy = sortErr (ieee_float_is_greater_or_equal [FValue ft,FValue vx, FValue vy])
                         "ieee-float-is-greater-or-equal not applied to a format and ieee-floats"


