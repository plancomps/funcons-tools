{-# LANGUAGE OverloadedStrings #-}

module Funcons.TypeSubstitution where

import Funcons.Types
import Funcons.Patterns

import qualified Data.Map as M
import qualified Data.Set as S

-- | Associates types (Terms) with meta-variables 
type TypeEnv = M.Map MetaVar TyAssoc
data TyAssoc = ElemOf FTerm | SubTyOf FTerm

-- | Version of `subsTypeVar` that does not replace the meta-variables
-- in the given set
limitedSubsTypeVar :: HasTypeVar a => S.Set MetaVar -> TypeEnv -> a -> a
limitedSubsTypeVar pvars env = subsTypeVar (foldr aux env pvars)
  where aux pvar = M.delete pvar

-- | Version of `subsTypeVarWildcard` that does not replace the meta-variables
-- in the given set
limitedSubsTypeVarWildcard :: HasTypeVar a => S.Set MetaVar -> Maybe FTerm -> TypeEnv -> a -> a
limitedSubsTypeVarWildcard pvars mt env = subsTypeVarWildcard mt (foldr aux env pvars)
  where aux pvar = M.delete pvar


-- | Used for replacing meta-variables `T` in pattern annotations `P:T` with
-- the type to which `T` is bound in some type-environment (if any)
class HasTypeVar a where
  subsTypeVar :: TypeEnv -> a -> a
  subsTypeVar = subsTypeVarWildcard Nothing

  subsTypeVarWildcard :: (Maybe FTerm) -> TypeEnv -> a -> a

instance HasTypeVar FTerm where
  subsTypeVarWildcard mt env t = case t of 
    TVar var -> case M.lookup var env of
                  Just (SubTyOf ty)   -> ty
                  Just (ElemOf (TSortSeq (TName "types") op)) -> TSortSeq (TName "values") op
                  Just (ElemOf (TSortSeq (TName "value-types") op)) -> TSortSeq (TName "values") op
                  Just (ElemOf (TName "types")) -> TName "values"
                  Just (ElemOf vt)  -> TVar var
                  Nothing           -> TVar var
    TName nm -> TName nm
    TApp nm ts-> TApp nm (map (subsTypeVarWildcard mt env) ts)
    TSeq ts -> TSeq (map (subsTypeVarWildcard mt env) ts)
--    TList ts  -> TList (map (subsTypeVarWildcard mt env) ts)
    TSet  ts  -> TSet (map (subsTypeVarWildcard mt env) ts)
    TMap ts   -> TMap (map (subsTypeVarWildcard mt env) ts)
    TBinding x y -> TBinding (subsTypeVarWildcard mt env x) (subsTypeVarWildcard mt env y)
    TFuncon f -> TFuncon f
    TSortSeq t op -> TSortSeq (subsTypeVarWildcard mt env t) op
    TSortPower t1 t2 -> TSortPower (subsTypeVarWildcard mt env t1) (subsTypeVarWildcard mt env t2)
    TSortUnion t1 t2 -> TSortUnion (subsTypeVarWildcard mt env t1) (subsTypeVarWildcard mt env t2)
    TSortInter t1 t2 -> TSortInter (subsTypeVarWildcard mt env t1) (subsTypeVarWildcard mt env t2)
    TSortComplement t -> TSortComplement (subsTypeVarWildcard mt env t)
    TSortComputes t -> TSortComputes (subsTypeVarWildcard mt env t)
    TSortComputesFrom f t -> TSortComputesFrom (subsTypeVarWildcard mt env f) (subsTypeVarWildcard mt env t)
    TAny -> case mt of  Nothing -> TAny
                        Just t  -> t

instance HasTypeVar VPattern where
  subsTypeVarWildcard mt env pat = case pat of 
    VPAnnotated p   t -> VPAnnotated (subsTypeVarWildcard mt env p) (subsTypeVarWildcard mt env t)
    PADT n pats       -> PADT n (map (subsTypeVarWildcard mt env) pats)
--    PList pats        -> PList (map (subsTypeVarWildcard mt env) pats)
    VPMetaVar var     -> VPMetaVar var
    VPSeqVar var op   -> VPSeqVar var op
    VPLit v           -> VPLit v
    VPWildCard        -> VPWildCard
    VPType pat        -> VPType pat 

instance HasTypeVar FPattern where
  subsTypeVarWildcard mt env pat = case pat of
    PAnnotated p t  -> PAnnotated (subsTypeVarWildcard mt env p) (subsTypeVarWildcard mt env t)
    PValue p        -> PValue $ subsTypeVarWildcard mt env p
    PMetaVar var    -> PMetaVar var
    PSeqVar var op  -> PSeqVar var op
    PWildCard       -> PWildCard

   
