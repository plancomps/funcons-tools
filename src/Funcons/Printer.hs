{-# LANGUAGE OverloadedStrings #-}

module Funcons.Printer (
    ppFuncons, ppValues, ppTypes, ppTerms,
    showValues, showFuncons, showTypes, showSorts, showTerms, showOp,
    showValuesSeq, ppValuesSeq, showFunconsSeq, ppFunconsSeq,
    ) where

import Funcons.Types
import Funcons.RunOptions

import Data.List (intercalate)
import Data.Text (unpack)

-- | Pretty-print a 'Values'.
showValues :: Values -> String
showValues = ppValues showFuncons 

-- | Pretty-print a sequence of `Values`.
showValuesSeq :: [Values] -> String
showValuesSeq = ppValuesSeq defaultRunOptions

showOp :: SeqSortOp -> String
showOp = ppOp

ppValuesSeq :: RunOptions -> [Values] -> String
ppValuesSeq opts = showArgs opts False . map FValue

-- | Pretty-print a 'Funcons'.
showFuncons :: Funcons -> String
showFuncons = ppFuncons defaultRunOptions

-- | Pretty-print a sequence of `Funcons`.
showFunconsSeq :: [Funcons] -> String
showFunconsSeq = ppFunconsSeq defaultRunOptions

ppFunconsSeq :: RunOptions -> [Funcons] -> String
ppFunconsSeq opts = showArgs opts False

-- | Pretty-print a 'Types'.
showTypes :: Types -> String
showTypes = ppTypes showFuncons 

-- | Pretty-print a sort or 'ComputationType'
showSorts :: ComputationTypes -> String
showSorts = ppComputationTypes showFuncons 

showTerms :: FTerm -> String
showTerms = ppTerms defaultRunOptions

ppFuncons :: RunOptions -> Funcons -> String
ppFuncons opts (FApp "list" fs) = "[" ++ showArgs opts False fs ++ "]"
--ppFuncons opts (FTuple fs)   = "(" ++ showArgs opts False fs ++ ")"
ppFuncons opts (FSet fs)     = "{" ++ showArgs opts False fs ++ "}"
ppFuncons opts (FMap fs)     = 
  "{" ++ intercalate "," (map (ppFuncons opts) fs) ++ "}"
ppFuncons opts (FBinding fk fvs) = 
  ppFuncons opts fk ++ " |-> " ++ ppFunconsSeq opts fvs
ppFuncons opts (FValue v)            = ppValues (ppFuncons opts) v
ppFuncons opts (FName nm)      = unpack nm
ppFuncons opts (FSortSeq f o)  = ppFuncons opts f ++ ppOp o
ppFuncons opts (FSortPower f1 f2) = ppFuncons opts f1 ++ "^" ++ ppFuncons opts f2
ppFuncons opts (FSortUnion f1 f2) = "(" ++ ppFuncons opts f1 ++ "|" ++ ppFuncons opts f2 ++ ")"
ppFuncons opts (FSortInter f1 f2) = "(" ++ ppFuncons opts f1 ++ "&" ++ ppFuncons opts f2 ++ ")"
ppFuncons opts (FSortComplement f1) = "~("++ ppFuncons opts f1 ++ ")"
ppFuncons opts (FSortComputes f) = "=>" ++ ppFuncons opts f
ppFuncons opts (FSortComputesFrom s t) = ppFuncons opts s ++ "=>" ++ ppFuncons opts t
-- some hard-coded funcons
ppFuncons opts (FApp "closure" [x, y]) =
    let env | pp_full_environments opts = y
            | otherwise                 = string_ "..."
    in showFn opts "closure" [x, env]
ppFuncons opts (FApp "scope" [x, y]) =
    let env | Prelude.not (pp_full_environments opts) && isMap x = string_ "..."
            | otherwise                                          = x
    in showFn opts "scope" [env, y]
ppFuncons opts (FApp nm fs)     = unpack nm ++ showArgs opts True fs 

-- helpers

showFn :: RunOptions -> String -> [Funcons] -> String
showFn opts n xs = n ++ showArgs opts True xs

showArgs :: RunOptions -> Bool -> [Funcons] -> String
showArgs opts b args | b         = "(" ++ seq ++ ")"
                     | otherwise = seq
 where seq = intercalate "," (map (ppFuncons opts) args)

ppTerms :: RunOptions -> FTerm -> String
ppTerms opts (TApp "list" ts)   = "[" ++ intercalate "," (map (ppTerms opts) ts) ++ "]"
ppTerms opts (TApp nm ts) = unpack nm ++ "(" ++ intercalate "," (map (ppTerms opts) ts) ++ ")"
ppTerms opts (TName nm)   = unpack nm
ppTerms opts (TVar var)   = var
ppTerms opts (TSeq ts)    = "(" ++ intercalate "," (map (ppTerms opts) ts) ++ ")"
ppTerms opts (TSet ts)    = "{" ++ intercalate "," (map (ppTerms opts) ts) ++ "}"
ppTerms opts (TMap ts)    = "map" ++ "(" ++ intercalate "," (map (ppTerms opts) ts) ++ ")"
ppTerms opts (TBinding fk fv) = ppTerms opts fk ++ " |-> " ++ ppTerms opts fv
ppTerms opts (TSortSeq term op) = ppTerms opts term ++ ppOp op
ppTerms opts (TSortPower t1 t2) = ppTerms opts t1 ++ "^" ++ ppTerms opts t2
ppTerms opts (TSortUnion t1 t2) = ppTerms opts t1 ++ "|" ++ show t2
ppTerms opts (TSortInter t1 t2) = ppTerms opts t1 ++ "&" ++ show t2
ppTerms opts (TSortComplement t1) = "~(" ++ ppTerms opts t1 ++ ")"
ppTerms opts (TSortComputes to) = "=>" ++ ppTerms opts to
ppTerms opts (TSortComputesFrom from to) = ppTerms opts from ++ "=>" ++ ppTerms opts to
ppTerms opts (TFuncon f)  = ppFuncons opts f
ppTerms opts TAny         = "_"


