{-# LANGUAGE OverloadedStrings, LambdaCase #-}

-- |
-- This module implements HGMPification of funcons based on Berger et al. (2017)
-- (First-order.)
module Funcons.MetaProgramming where

import Funcons.MSOS
import Funcons.EDSL
import Funcons.Types
import Funcons.Patterns
import Funcons.RunOptions
import Funcons.Simulation

import Data.Maybe (fromJust)
import Data.Text (pack, unpack)
import qualified Data.Map as M
import qualified Data.Set as S

-- | This function implements the ==CT=> relation.
-- Compiling programs to executable funcons terms,
-- removing occurrences of `meta-up`, `meta-down` and `meta-let` and `meta-bound`.
ctRel :: Funcons -> MSOS Funcons
ctRel f = case f of
  FName nm                        -> return f 
  FApp "meta-up" [m]              -> ulRel m
  FApp "meta-down" [m]            -> staticEval m
  FApp "meta-let" [FValue nm,m,n] | isString_ nm -> do 
    v <- evalRel =<< ctRel m
    [menv] <- getInh env_entity 
    let env' = case menv of 
                Map env -> Map (M.insert nm [v] env)
                _       -> Map (M.singleton nm [v])
    withInh env_entity [env'] (ctRel n)
  FApp nm arg                     -> FApp nm <$> mapM ctRel arg
--  FList fs                        -> FList <$> mapM ctRel fs
  FSet fs                         -> FSet <$> mapM ctRel fs
  FMap fs                         -> FMap <$> mapM ctRel fs
  FValue (ADTVal nm fs)           -> FValue . ADTVal nm <$> mapM ctRel fs
  FValue (ComputationType (Type (ADT nm fs)))
                                  -> FValue . ComputationType . Type . ADT nm <$> mapM ctRel fs
  FValue v                        -> return (FValue v)
  _                               -> liftRewrite (sortErr f ("ctRel not defined"))
  where staticEval m = ctRel m >>= evalRel >>= liftRewrite . dlRel  

-- | This function implements the ==UL=> relation.
-- Translating a funcon into its meta-representation
ulRel :: Funcons -> MSOS Funcons
ulRel f = case f of
  FName nm              -> return $ ast_term [string_ (unpack nm)]
  FApp "meta-down" [f]  -> ctRel f
  FApp "meta-up" [m]    -> ulRel m >>= ulRel
  FApp nm fs            -> ast_term . (string_ (unpack nm):) <$> mapM ulRel fs
--  FList fs              -> ast_term . (string_ "list":) <$> mapM ulRel fs
  FSet fs               -> ast_term . (string_ "set":) <$> mapM ulRel fs
  FMap fs               -> ast_term . (string_ "map":) <$> mapM ulRel fs
  FValue v              -> return $ ast_value [type_ (tyOf v), FValue v]
  -- What TODO with type annotations? 
  _                     -> liftRewrite (sortErr f ("ulRel not defined"))

-- | This function implements the ==DL=> relation.
-- Translating a meta-representation of a program into the actual program
dlRel :: Values -> Rewrite Funcons
dlRel v = case v of 
  ADTVal "ast-value" [t,v] | Just (ComputationType _) <- project t -> return v
  ADTVal "ast-term" [nm] | isString nm -> return (FName (pack (unString (fromJust (project nm)))))
  ADTVal "ast-term" (s:vs')
    | Just vs <- mapM project vs' 
    , isString s, "set" <- unString (fromJust (project s)) -> FSet <$> mapM dlRel vs
  ADTVal "ast-term" (m:vs')
    | Just vs <- mapM project vs' 
    , isString m, "map" <- unString (fromJust (project m)) -> FMap <$> mapM dlRel vs
  ADTVal "ast-term" (nm:vs')       
    | Just vs <- mapM project vs' 
    , isString nm -> FApp (pack (unString (fromJust (project nm)))) <$> 
                      mapM dlRel vs
  _ -> sortErr (meta_down_ (fvalues [v])) "meta-down not applied to a meta-representation"

evalRel :: Funcons -> MSOS Values
evalRel f = evalFuncons f >>= \case 
  Right [v]   -> return v
  Right vs    -> liftRewrite $ internal "meta evaluation yields a sequence of values"
  Left f'     -> evalRel f'
  where setGlobal f ctxt = ctxt { ereader = (ereader ctxt) { global_fct = f } }

compile :: FunconLibrary -> TypeRelation -> Funcons -> Funcons -> Funcons
compile lib tyenv fenv f = 
  case runSimIO (runMSOS process (cmp_MSOSReader lib tyenv f) cmp_MSOSState) M.empty of
    ((Left ie , _,_), _) -> error ("failed to compile\n" ++ showIException ie)
    ((Right f, _, _), _) -> f  
  where process = do  
          env <- evalRel fenv
          putMut atom_gen_entity (Set S.empty)
          putMut store_entity (Map M.empty)
          withInh env_entity [env] (ctRel f)

env_entity = "environment"
store_entity = "store"
atom_gen_entity = "used-atom-set"
 
translationStep :: ([Funcons] -> Funcons) -> StrictFuncon
translationStep f vs = compstep $ do  fs <- liftRewrite (mapM dlRel vs)
                                      Left <$> ulRel (f fs)

cmp_MSOSReader lib env f = MSOSReader cmp_RewriteReader M.empty M.empty (fread True)
  where cmp_RewriteReader = RewriteReader lib env defaultRunOptions f f  
--cmp_MSOSWriter = MSOSWriter mempty mempty mempty
cmp_MSOSState = MSOSState M.empty M.empty (RewriteState)

library :: FunconLibrary
library = libFromList [
--    ("meta-up", NonStrictFuncon step_meta_up)  -- static funcon
--  , ("meta-down", StrictFuncon step_meta_down) -- static funcon
    ("eval", StrictFuncon step_meta_eval)
  , ("code", NonStrictFuncon step_code)
  , ("type-of", StrictFuncon step_ty_of)
  ]

meta_up_ = applyFuncon "meta-up"
meta_down_ = applyFuncon "meta-down"
meta_let_ = applyFuncon "meta-let"

eval_ = applyFuncon "eval"
code_ = applyFuncon "code"
step_meta_eval :: [Values] -> Rewrite Rewritten
step_meta_eval [v] = dlRel v >>= rewriteTo
step_meta_eval fs = sortErr (eval_ (fvalues fs)) "eval not applied to one argument"

step_code :: [Funcons] -> Rewrite Rewritten
step_code [f] = compstep (toStepRes <$> ulRel f)
step_code fs = sortErr (code_ fs) "code not applied to a single term"

ast_term = applyFuncon "ast-term"
ast_value = applyFuncon "ast-value"

type_of_ = applyFuncon "type-of"
step_ty_of :: [Values] -> Rewrite Rewritten
step_ty_of [v] = rewriteTo $ type_ $ tyOf v
step_ty_of vs = sortErr (type_of_ (fvalues vs)) "type-of not applied to a single value"

--TODO perhaps the parser should be extended to recognise "ast-" prefixes
-- which are translated into applications of VMeta 
