{-# LANGUAGE OverloadedStrings  #-}

-- | 
-- This module provides the types and the functions necessary for defining funcons.
-- The package provides a large collection of predefined funcons in "Funcons.Core".
-- Module "Funcons.Tools" provides functions for creating executables.
module Funcons.EDSL (
    -- * Funcon representation
        Funcons(..), Values(..), Types(..), ComputationTypes(..),SeqSortOp(..),
            applyFuncon,
    -- ** Smart construction of funcon terms
        app0_, app1_, app2_, app3_,
    -- *** Funcon terms
        set_, vec_, env_fromlist_, null__,
    -- *** Values
        int_, bool_, bool__, list__, vector__, tuple__, char_, char__, nat_, float_, ieee_float_32_, ieee_float_64_, string_, string__, atom_,
    -- *** Types
        values_, integers_, vectors_, type_, ty_star, ty_plus, ty_opt, ty_union, ty_neg, ty_inter, ty_power,
    -- ** Pretty-print funcon terms
        showValues, showValuesSeq, showFuncons, showFunconsSeq, showTypes, showTerms, showOp,
    -- ** Is a funcon term a certain value?
        isVal, isInt, isNat, isList, isMap, isType,
        isVec, isChar, isTup, isString, isString_, unString, 
    -- ** Up and downcasting between funcon terms 
        downcastValue, downcastType, downcastValueType,
        upcastNaturals, upcastIntegers, upcastRationals,upcastCharacter, 
    -- ** Evaluation functions
         EvalFunction(..), Strictness(..), StrictFuncon, PartiallyStrictFuncon, 
              NonStrictFuncon, ValueOp, NullaryFuncon,

    -- *** Funcon libraries
    FunconLibrary, libEmpty, libUnion, libOverride, libUnions, libOverrides, libFromList, Funcons.EDSL.library, fromNullaryValOp, fromValOp, fromSeqValOp,
    -- ** Implicit & modular propagation of entities
    MSOS, Rewrite, Rewritten, 
    -- *** Helpers to create rewrites & step rules
            rewriteTo, rewriteSeqTo, stepTo, stepSeqTo, 
              compstep, rewritten, premiseStep, premiseEval,
                norule, sortErr, partialOp,
    -- *** Entities and entity access
        Inherited, getInh, withInh,
        Mutable, getMut, putMut,
        Output, writeOut, readOut, 
        Control, raiseSignal, receiveSignals, 
        Input, withExtraInput, withExactInput,  
            
    -- * CBS compilation

    -- $cbsintro

    -- ** Funcon representation with meta-variables
        FTerm(..), Env, emptyEnv, fvalues,
    -- *** Defining rules 
        rewriteTermTo,stepTermTo,premise,
    -- *** Entity access
        withInhTerm, getInhPatt, putMutTerm, getMutPatt, writeOutTerm, readOutPatt, 
        receiveSignalPatt, raiseTerm, matchInput, withExtraInputTerms, withExactInputTerms,
        withControlTerm, getControlPatt,
    -- ** Backtracking
        evalRules, stepRules, rewriteRules,
           SideCondition(..), sideCondition,  lifted_sideCondition,
    -- ** Pattern Matching
        VPattern(..), FPattern(..), TPattern(..), 
            vsMatch, fsMatch, f2vPattern,
            lifted_vsMatch, lifted_fsMatch, pat2term, vpat2term, typat2term,
    -- ** Meta-environment
        envRewrite, envStore, lifted_envRewrite, lifted_envStore,
      
    -- * Type substitution
    TypeEnv, TyAssoc(..), HasTypeVar(..), limitedSubsTypeVar, limitedSubsTypeVarWildcard, 
 
    -- * Tools for creating interpreters
  
    -- For more explanation see "Funcons.Tools"
    -- ** Helpers for defining evaluation functions.
        rewriteType,
    -- ** Default entity values
        EntityDefaults, EntityDefault(..),
    -- ** Type environments
        TypeRelation, TypeParam(..), DataTypeMembers(..), DataTypeAltt(..), 
            typeLookup, typeEnvUnion, typeEnvUnions, typeEnvFromList, emptyTypeRelation,
    )where

import Funcons.MSOS
import Funcons.Types
import qualified Funcons.Operations as VAL
import Funcons.Entities
import Funcons.Patterns
import Funcons.Substitution
import Funcons.Printer
import Funcons.TypeSubstitution

import Control.Arrow ((***))

congruence1_1 :: Name -> Funcons -> Rewrite Rewritten
congruence1_1 fnm = compstep . premiseStepApp (flattenApp app) 
    where app = applyFuncon fnm

congruence1_2 :: Name -> Funcons -> Funcons -> Rewrite Rewritten
congruence1_2 fnm arg1 arg2 = compstep $ premiseStepApp (flattenApp app) arg1 
    where app fs = applyFuncon fnm (fs++[arg2])

congruence2_2 :: Name -> Funcons -> Funcons -> Rewrite Rewritten
congruence2_2 fnm arg1 arg2 = compstep $ premiseStepApp (flattenApp app) arg2
    where app fs = applyFuncon fnm (arg1:fs)

congruence1_3 :: Name -> Funcons -> Funcons -> Funcons -> Rewrite Rewritten
congruence1_3 fnm arg1 arg2 arg3 = compstep $ premiseStepApp (flattenApp app) arg1 
    where app fs = applyFuncon fnm (fs ++ [arg2, arg3])

congruence2_3 :: Name -> Funcons -> Funcons -> Funcons -> Rewrite Rewritten
congruence2_3 fnm arg1 arg2 arg3 = compstep $ premiseStepApp (flattenApp app) arg2 
    where app fs = applyFuncon fnm (arg1 : fs ++ [arg3])

congruence3_3 :: Name -> Funcons -> Funcons -> Funcons -> Rewrite Rewritten
congruence3_3 fnm arg1 arg2 arg3 = compstep $ premiseStepApp (flattenApp app) arg3 
    where app fs = applyFuncon fnm ([arg1, arg2] ++ fs)

flattenApp :: ([Funcons] -> Funcons) -> (StepRes -> StepRes) 
flattenApp app res = case res of 
  Left f    -> toStepRes (app [f])
  Right vs  -> toStepRes (app (map FValue vs))

-- | Create an environment from a list of bindings (String to Values)
-- This function has been introduced for easy expression of the
-- semantics of builtin identifiers 
env_fromlist_ :: [(String, Funcons)] -> Funcons
env_fromlist_ = FMap . map (\(k,v) -> tuple_ [string_ k, v])


-- | A funcon library with funcons for builtin types.
library :: FunconLibrary
library = libUnions [unLib, nullLib, binLib, floatsLib,boundedLib]
 where
        nullLib = libFromList (map (id *** mkNullary) nullaryTypes)
        unLib   = libFromList (map (id *** mkUnary) unaryTypes)
        binLib  = libFromList (map (id *** mkBinary) binaryTypes)
        floatsLib = libFromList (map (id *** mkFloats) floatTypes)
        boundedLib = libFromList (map (id *** mkBounded) boundedIntegerTypes)

        mkNullary :: Types -> EvalFunction 
        mkNullary = NullaryFuncon . rewritten . typeVal

        mkFloats :: (IEEEFormats -> Types) -> EvalFunction
        mkFloats cons = StrictFuncon sfuncon
            where   sfuncon [ADTVal "binary32" _] = rewritten $ typeVal $ cons Binary32
                    sfuncon [ADTVal "binary64" _] = rewritten $ typeVal $ cons Binary64
                    sfuncon vs = sortErr (tuple_val_ vs) "ieee-float not applied to ieee-format"
        mkBounded :: (Integer -> Types) -> EvalFunction
        mkBounded cons = StrictFuncon sfuncon
            where   sfuncon [v1] 
                        | Int i1 <- upcastIntegers v1 = 
                                    rewritten $ typeVal $ cons i1
                    sfuncon v = sortErr (tuple_val_ v) "type not applied to an integer value" 

        mkUnary :: (Types -> Types) -> EvalFunction
        mkUnary cons = StrictFuncon sfuncon
            where sfuncon [ComputationType (Type x)]  = rewritten $ typeVal $ cons x
                  sfuncon  _                          = rewritten $ typeVal $ cons VAL.Values

        mkBinary :: (Types -> Types -> Types) -> EvalFunction
        mkBinary cons = StrictFuncon sfuncon
            where sfuncon [ComputationType (Type x), ComputationType (Type y)] = 
                    rewritten $ typeVal $ maps (injectT x) (injectT y)
                  sfuncon _ = rewritten $ typeVal $ cons VAL.Values VAL.Values

app0_ :: ([Funcons] -> Funcons) -> Funcons
app0_ cons = cons []

app1_ :: ([Funcons] -> Funcons) -> Funcons -> Funcons
app1_ cons x = cons [x]

app2_ :: ([Funcons] -> Funcons) -> Funcons -> Funcons -> Funcons
app2_ cons x y = cons [x,y]

app3_ :: ([Funcons] -> Funcons) -> Funcons -> Funcons -> Funcons -> Funcons
app3_ cons x y z = cons [x,y,z]


ty_star,ty_opt,ty_plus,ty_neg :: Funcons -> Funcons
ty_star = flip FSortSeq StarOp
ty_opt = flip FSortSeq QuestionMarkOp
ty_plus = flip FSortSeq PlusOp
ty_neg = FSortComplement 

ty_inter,ty_union,ty_power :: Funcons -> Funcons -> Funcons
ty_inter = FSortInter
ty_union = FSortUnion
ty_power = FSortPower

-- $cbsintro
-- This section describes functions that extend the interpreter with
-- backtracking and pattern-matching facilities. These functions
-- are developed for compiling CBS funcon specifications to 
-- Haskell. To read about CBS we refer to 
-- <http://plancomps.dreamhosters.com/wp-content/uploads/2016/02/jlamp-16.pdf JLAMP2016>. 
-- The functions can be 
-- used for manual development of funcons, although this is not recommended.
