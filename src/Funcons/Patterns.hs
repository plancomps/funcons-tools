{-# LANGUAGE LambdaCase, StandaloneDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Patterns where

import Funcons.MSOS
import Funcons.Types
import Funcons.Substitution
import Funcons.Exceptions
import Funcons.Operations (isGround)

import Control.Applicative
import Control.Monad (foldM, forM)
import Data.Function (on)
import Data.List (sortBy, intercalate)
import Data.Monoid
import Data.Text (unpack)
import Data.Foldable (toList)
import qualified Data.Map as M 

-- pattern matching
type Matcher a = [a] -> Int -> Env -> Rewrite [(Int, Env)]
type SeqVarInfo = (MetaVar, SeqSortOp, Maybe FTerm)

singleMatcher :: (a -> b -> Env -> Rewrite Env) -> b -> Matcher a
singleMatcher p pat str k env = case drop k str of
    []  -> return []
    f:_ -> eval_catch (p f pat env) >>= \case
            Left ie | failsRule ie  -> return []
                    | otherwise     -> rewrite_rethrow ie
            Right env'              -> return [(k+1,env')]

seqMatcher :: (a -> Maybe FTerm -> Env -> Rewrite (Maybe [a])) -> ([a] -> Levelled) 
                -> SeqVarInfo -> Matcher a
seqMatcher p level (var, op, mty) str k env = case op of
    QuestionMarkOp -> makeResults ((<=1) . length)
    PlusOp         -> case str of  
                        [] -> return []
                        _  -> makeResults ((>=1) . length)
    StarOp         -> makeResults (const True)
    where makeResults filter_op = do
            furthest <- takeWhileM (\a -> p a mty env) (drop k str)
            return (sortWithPref $ map ins (filter filter_op $ ordered_subsequences furthest))
            where
              ins fs  = (k+length fs, envInsert var (level (concat fs)) env)

              takeWhileM :: (a -> Rewrite (Maybe [a])) -> [a] -> Rewrite [[a]]
              takeWhileM _ [] = return []
              takeWhileM p (x:xs) = eval_catch (p x) >>= \case
                Right (Just fs)     -> (fs :) <$> takeWhileM p xs
                Right Nothing       -> return []
                Left ie | failsRule ie  -> return []
                        | otherwise     -> rewrite_rethrow ie

              -- sorts the result in order of preference
              sortWithPref :: [(Int, Env)] -> [(Int, Env)] 
              sortWithPref = sortBy (comparison `on` fst)
                where comparison = case mty of
                        Nothing   -> compare      -- no annotation => shortest match
                        Just  _   -> flip compare -- annoration => longest match

matching :: (a -> String) -> [a] -> [Matcher a] -> Env -> Rewrite Env
matching show str ps env = do 
    matches <- foldr seqWith lastMatcher ps str 0 env 
    let rule_fail = PatternMismatch ("Pattern match failed: " ++ intercalate "," (map show str))
    case matches of
        [] -> rewrite_throw rule_fail
        [(_,env')] -> return env'
        _  -> internal ("ambiguity not resolved") 
    where   m = length str
            -- sequencing of matchers specifically to disambiguate safely
            lastMatcher :: Matcher a
            lastMatcher _ k env | k == m       = return [(k,env)]
                                | otherwise    = return []

            seqWith :: Matcher a -> Matcher a -> Matcher a
            seqWith p q str k env = p str k env >>= foldM acceptFirst []
             where  acceptFirst acc (r, env)
                        | null acc  = q str r env
                        | otherwise = return acc

ordered_subsequences :: [a] -> [[a]]
ordered_subsequences xs = ordered_subsequences' xs []
    where   ordered_subsequences' [] acc = [acc]
            ordered_subsequences' (x:xs) acc = acc : ordered_subsequences' xs (acc++[x])


-- | Patterns for matching funcon terms ('FTerm').
data FPattern   = PValue VPattern
                | PMetaVar MetaVar 
                | PSeqVar MetaVar SeqSortOp
                | PAnnotated FPattern FTerm 
                | PWildCard
                deriving (Show, Eq, Ord, Read)

f2vPattern :: FPattern -> VPattern
f2vPattern (PValue v) = v
f2vPattern (PMetaVar var) = VPMetaVar var
f2vPattern (PSeqVar var op) = VPSeqVar var op
f2vPattern (PAnnotated fp t) = VPAnnotated (f2vPattern fp) t
f2vPattern PWildCard = VPWildCard

-- | Patterns for matching values ('Values').
data VPattern   = PADT Name [VPattern]
                | VPWildCard
--                | PList [VPattern]
                | VPMetaVar MetaVar 
                | VPAnnotated VPattern FTerm 
                | VPSeqVar MetaVar SeqSortOp
                | VPLit Values 
                | VPType TPattern
                deriving (Show, Eq, Ord, Read)

v2tPattern :: VPattern -> Maybe TPattern
v2tPattern (VPType t) = Just t
v2tPattern (VPMetaVar var) = Just $ TPVar var
v2tPattern (VPSeqVar var op) = Just $ TPSeqVar var op
v2tPattern (PADT cons pats) = TPADT cons <$> mapM v2tPattern pats
v2tPattern (VPLit lit) = Just $ TPLit (TFuncon (FValue lit))
v2tPattern VPWildCard = Just TPWildCard
--v2tPattern (PList _) = Nothing  
v2tPattern (VPAnnotated fp t) = Nothing 

-- required for "matching" lazy arguments (fields) of adt constructors
v2fPattern :: VPattern -> FPattern
v2fPattern (VPMetaVar x)          = PMetaVar x
v2fPattern (VPSeqVar x op)        = PSeqVar x op
v2fPattern (VPAnnotated pat ann)  = PAnnotated (v2fPattern pat) ann
v2fPattern VPWildCard             = PWildCard
v2fPattern vp                     = PValue vp

substitute_patt_signal :: VPattern -> Env -> Rewrite VPattern
substitute_patt_signal vpat env = case vpat of 
  PADT name vpats -> PADT name <$> subs_flatten vpats env
  VPWildCard      -> return VPWildCard
--  PList pats      -> PList <$> subs_flatten pats env
  VPMetaVar var   -> case M.lookup var env of
                      Nothing -> return (VPMetaVar var)
                      Just v  -> VPLit <$> vLevel v
  VPAnnotated vpat ann  -> VPAnnotated <$> substitute_patt_signal vpat env 
                                       <*> return ann 
  VPSeqVar var op -> case M.lookup var env of
                      Nothing -> return (VPSeqVar var op)
                      Just v  -> VPLit <$> vLevel v 
  VPLit lit       -> return $ VPLit lit
  VPType tpat     -> return $ VPType tpat -- assumes type patterns do not occur in signals
  where subs_flatten :: [VPattern] -> Env -> Rewrite [VPattern]
        subs_flatten terms env = (concat <$>) $ forM terms $ \case
              vpat@(VPMetaVar k)   -> envMaybeLookup env k vpat
              vpat@(VPSeqVar k op) -> envMaybeLookup env k vpat
              vpat                 -> (:[]) <$> substitute_patt_signal vpat env

        envMaybeLookup :: Env -> MetaVar -> VPattern -> Rewrite [VPattern] 
        envMaybeLookup env k vpat = case M.lookup k env of
                            Nothing -> return [vpat]
                            Just lf -> map VPLit <$> vsLevel lf


-- | Variant of 'vsMatch' that is lifted into the 'MSOS' monad.
lifted_vsMatch str pats env = liftRewrite $ vsMatch str pats env
-- | Matching values with value patterns patterns.
-- If the list of patterns is a singleton list, then 'vsMatch' attempts
-- to match the values as a tuple against the pattern as well.
vsMatch :: [Values] -> [VPattern] -> Env -> Rewrite Env 
vsMatch str pats env = strict_vsMatch str pats env
{-case pats of
    [pat]   -> do
        e_ie_env <- eval_catch (strict_vsMatch str [pat] env)
        case e_ie_env of
            Right env' -> return env'
            Left ie | failsRule ie -> vMatch (safe_tuple_val str) pat env
                    | otherwise -> rewrite_rethrow ie
    _       -> strict_vsMatch str pats env
-}

-- | Match stricly values with patterns.
strict_vsMatch :: [Values] -> [VPattern] -> Env -> Rewrite Env
strict_vsMatch str pats env 
  | all isSort_ str 
  , Just tpats <- sequence (map v2tPattern pats)
        = tsMatch (map (downcastSort . FValue) str) tpats env 
  | otherwise = matching showValues str matchers env
        where   matchers = map toMatcher pats
                toMatcher pat = case vpSeqVarInfo pat of
                    Just info   -> seqMatcher isInMaybeTermTypePreserve ValuesTerm info
                    Nothing     -> singleMatcher vMatch pat 

-- | Variant of 'premiseStep' that applies substitute and pattern-matching.
premise :: FTerm -> [FPattern] -> Env -> MSOS Env
premise x pats env = liftRewrite (subsAndRewritesInEnv x env) >>= \case
  (ValTerm v, env')       -> msos_throw (StepOnValue v)
  (CompTerm f step,env')  -> do 
      ef' <- count_delegation >> optRefocus step 
      case ef' of Left f'   -> liftRewrite $ fsMatch [f'] pats env'
                  Right vs' -> liftRewrite $ fsMatch (map FValue vs') pats env'

-- | Variant of 'fsMatch' that is lifted into the 'MSOS' monad.
-- If all given terms are values, then 'vsMatch' is used instead.
lifted_fsMatch str pats env = liftRewrite $ fsMatch str pats env
-- | Match a sequence of terms to a sequence of patterns.
fsMatch = fsMatchStrictness False
strict_fsMatch = fsMatchStrictness True
fsMatchStrictness :: Bool -> [Funcons] -> [FPattern] -> Env -> Rewrite Env
fsMatchStrictness strict str pats env 
    -- if all the given funcons are values, then perform value matching instead.
    | not strict && all (not.hasStep) str = 
          let downValues vs = map downcastValue vs
          in vsMatch (downValues str) (map f2vPattern pats) env
    | otherwise = matching showFuncons str matchers env
    where   matchers = map toMatcher pats
            toMatcher pat = case fpSeqVarInfo pat of
                Just info   -> seqMatcher rewritesToAnnotatedType FunconsTerm info
                Nothing     -> singleMatcher fMatch pat

fMatch :: Funcons -> FPattern -> Env -> Rewrite Env 
fMatch _ PWildCard env = return env
fMatch f (PMetaVar var) env = return (envInsert var (FunconTerm f Nothing) env)
fMatch f (PAnnotated pat term) env = do
    tys <- subsAndRewritesToValues term env
    let fail = rewrite_throw (PatternMismatch ("pattern annotation check failed: " ++ showValuesSeq tys))
    rewriteFunconsWcount f >>= \case  
        ValTerm vs  -> do   b <- isInTuple vs tys 
                            if b then vsMatch vs [f2vPattern pat] env
                                 else fail
        otherwise   -> fail 
-- * a sequence variable can match the singleton sequence 
fMatch f pat@(PSeqVar _ _) env = fsMatch [f] [pat] env
-- if the pattern is a value attempt evaluation by rewrite
fMatch f (PValue pat) env = rewriteFunconsWcount f >>= 
    \case   ValTerm vs -> vsMatch vs [pat] env
            CompTerm _ _ -> rewrite_throw --important, should remain last 
                (PatternMismatch ("could not rewrite to value: " ++ showFuncons f))

lifted_fMaybeMatch mf mp env = liftRewrite $ fMaybeMatch mf mp env
fMaybeMatch Nothing Nothing env = return env
fMaybeMatch (Just f) (Just p) env = fMatch f p env
fMaybeMatch _ _ env = rewrite_throw (PatternMismatch "fMaybeMatch")

lifted_vMaybeMatch mv mp env = liftRewrite $ vMaybeMatch mv mp env
vMaybeMatch :: Maybe Values -> Maybe VPattern -> Env -> Rewrite Env
vMaybeMatch Nothing Nothing env = return env
vMaybeMatch (Just v) (Just p) env = vMatch v p env
vMaybeMatch _ _ env = rewrite_throw (PatternMismatch ("vMaybeMatch")) 

lifted_vMatch v p env = liftRewrite $ vMatch v p env
vMatch :: Values -> VPattern -> Env -> Rewrite Env
-- builtin special case for builtin value constructor `datatype-value`
vMatch (ADTVal str vs) (PADT "datatype-value" pats) env = 
  adtMatch "" "" (string_ (unpack str):vs) pats env
vMatch _ (VPWildCard) env = return env
vMatch VAny _ env = return env
vMatch v (VPMetaVar var) env = return (envInsert var (ValueTerm v) env)
vMatch (ADTVal str vs) (PADT con pats) env = adtMatch str con vs pats env
-- strict because we do not want to match the sequence "inside" the list
--vMatch (List vs) (PList ps) env = strict_vsMatch vs ps env 
vMatch v (VPAnnotated pat term) env = do
    ty <- subsAndRewritesToValue term env
    isIn v ty >>= \case
        True  -> vMatch v pat env
        False -> rewrite_throw (PatternMismatch ("pattern annotation check failed: " ++ showValues ty))
vMatch v (VPLit v2) env | v == v2 = return env
-- special treatment for sequence variables:
-- * a (single) sequence variable can match a tuple
-- * a sequence variable can match the singleton sequence
vMatch v pat@(VPSeqVar _ _) env = vsMatch [v] [pat] env
-- * a single value can match a tuple of patterns if it contains sequences
vMatch (ComputationType ty) (VPType pat) env = tMatch ty pat env
vMatch v _ _ = rewrite_throw (PatternMismatch ("failed to match"))

tsMatch :: [ComputationTypes] -> [TPattern] -> Env -> Rewrite Env
tsMatch = strict_tsMatch

strict_tsMatch :: [ComputationTypes] -> [TPattern] -> Env -> Rewrite Env
strict_tsMatch str pats env = matching showSorts str matchers env
        where   matchers = map toMatcher pats
                toMatcher pat = case tpSeqVarInfo pat of
                  Nothing -> singleMatcher tMatch pat 
                  Just info -> seqMatcher isInMaybeTypeTypePreserve TypesTerm info

tMatch :: ComputationTypes -> TPattern -> Env -> Rewrite Env
tMatch t p env = case p of
  TPWildCard -> return env
  TPVar x -> return (envInsert x (TypeTerm t) env)
  TPSeqVar _ _ -> tsMatch [t] [p] env
  TPLit ft -> subsAndRewritesToValue ft env >>= \case 
    ComputationType ty ->   
      if t == ty then return env 
                 else rewrite_throw (PatternMismatch ("failed to match"))
    _ -> internal "type-pattern literal not a type"
  TPComputes fp | ComputesType ft <- t -> tMatch (Type ft) fp env
  TPComputesFrom fp tp | ComputesFromType ft tt <- t -> 
    tMatch (Type ft) fp env >>= tMatch (Type tt) tp 
  TPADT nm ps | Type (ADT nm' ts) <- t,  nm == nm' ->
    fsMatch ts (map (PValue . VPType) ps) env -- TODO change ps to value pattern (also in generator, see other TODO)
  _ -> rewrite_throw (PatternMismatch ("failed to match"))

adtMatch :: Name -> Name -> [Funcons] -> [VPattern] -> Env -> Rewrite Env
adtMatch con pat_con vs vpats env 
    | con /= pat_con = rewrite_throw (PatternMismatch ("failed to match constructors: (" ++ show (con,pat_con) ++ ")"))
    | otherwise = fsMatch vs (map v2fPattern vpats) env


fpSeqVarInfo :: FPattern -> Maybe SeqVarInfo
fpSeqVarInfo (PSeqVar var op) = Just (var, op, Nothing)
fpSeqVarInfo (PAnnotated (PSeqVar var op) term) = Just (var, op, Just term)
fpSeqVarInfo _ = Nothing

vpSeqVarInfo :: VPattern -> Maybe SeqVarInfo 
vpSeqVarInfo (VPSeqVar var op) = Just (var, op, Nothing)
vpSeqVarInfo (VPAnnotated (VPSeqVar var op) term) = Just (var, op, Just term)
vpSeqVarInfo _ = Nothing

tpSeqVarInfo :: TPattern -> Maybe SeqVarInfo
tpSeqVarInfo (TPSeqVar var op) = Just (var, op, Nothing)
tpSeqVarInfo _ = Nothing

-- | CSB supports five kinds of side conditions.
-- Each of the side conditions are explained below.
-- When a side condition is not accepted an exception is thrown that 
-- is caught by the backtrackign procedure 'evalRules'.
-- A value is a /ground value/ if it is not a thunk (and not composed out of
--  thunks).
data SideCondition  
    -- | /T1 == T2/. Accepted only when /T1/ and /T2/ rewrite to /equal/ ground values.
    = SCEquality FTerm FTerm 
    -- | /T1 =\/= T2/. Accepted only when /T1/ and /T2/ rewrite to /unequal/ ground values.
    | SCInequality FTerm FTerm 
    -- | /T1 : T2/. Accepted only when /T2/ rewrites to a type and /T1/ rewrites to a value of that type.
    | SCIsInSort FTerm FTerm
    -- | /~(T1 : T2)/. Accepted only when /T2/ rewrites to a type and /T1/ rewrites to a value /not/ of that type.
    | SCNotInSort FTerm FTerm
    -- | /T = P/. Accepted only when /T/ rewrites to a value that matches the pattern /P/. (May produce new bindings in 'Env').
    | SCPatternMatch FTerm [VPattern]

-- | Variant of 'sideCondition' that is lifted into the 'MSOS' monad.
lifted_sideCondition sc env = liftRewrite $ sideCondition sc env

-- | Executes a side condition, given an 'Env' environment, throwing possible exceptions, and 
-- possibly extending the environment.
sideCondition :: SideCondition -> Env -> Rewrite Env
sideCondition cs env = case cs of
    SCEquality term1 term2 -> 
        prop "equality condition" (lift allEqual) term1 term2 env
    SCInequality term1 term2 ->     
        prop "inequality condition" (lift allUnEqual) term1 term2 env
    SCIsInSort term1 term2 -> prop "sort annotation" isInTuple term1 term2 env
    SCNotInSort term1 term2 -> 
        prop "neg. sort annotation" (\a b -> isInTuple a b >>= return . not) term1 term2 env
    SCPatternMatch term vpats -> 
      eval_catch (subsAndRewritesInEnv term env) >>= \case 
            -- env' binds term to step or value, if possible 
            --  courtesy of subsAndRewritesInEnv
            Right (ValTerm vs, env')        -> vsMatch vs vpats env'
            Right (CompTerm lf step, env')  -> case vpats of 
              [VPMetaVar v] -> return (envInsert v (FunconTerm lf (Just step)) env')
              _             -> fsMatch [lf] pats env'
            Left (_,lf,PartialOp _)     -> fsMatch [lf] pats env
            Left ie                     -> rewrite_rethrow ie
      where pats = map toFPat vpats
              where toFPat vpat = case vpat of
                      VPMetaVar var   -> PMetaVar var 
                      value_pat       -> PValue value_pat
  where prop :: String -> ([Values] -> [Values] -> Rewrite Bool) -> FTerm -> FTerm -> Env -> Rewrite Env
        prop msg op term1 term2 env = do
            (vs1,env1) <- subsAndRewritesToValuesInEnv term1 env
            (vs2,env2) <- subsAndRewritesToValuesInEnv term2 env1
            b <- op vs1 vs2
            if b then return env2
                 else rewrite_throw (SideCondFail (msg ++ " fails with " ++ showValuesSeq vs1 ++ " and " ++ showValuesSeq vs2))
        lift op xs ys = return (op xs ys)
      
-- piggy back on 
matchTypeParams :: [ComputationTypes] -> [TypeParam] -> Rewrite Env
matchTypeParams tys tparams = 
    let param_pats = map mkPattern tparams
         where mkPattern (Nothing, _, kind)  = VPAnnotated VPWildCard kind
               mkPattern (Just var, Nothing, kind) = VPAnnotated (VPMetaVar var) kind
               mkPattern (Just var, Just op, kind) = VPAnnotated (VPSeqVar var op) kind
    in vsMatch (map ComputationType tys) param_pats emptyEnv 


alwaysAccept :: Funcons -> Maybe FTerm -> Env -> Rewrite Bool
alwaysAccept _ _ _ = return True

rewritesToAnnotatedType :: Funcons -> Maybe FTerm -> Env -> Rewrite (Maybe [Funcons])
rewritesToAnnotatedType f Nothing _ = return (Just [f])
rewritesToAnnotatedType f (Just term) env = rewriteFunconsWcount f >>= \case 
  ValTerm [v]   -> isInMaybeTermType v (Just term) env >>= \case
                      True  -> return (Just [FValue v])
                      _     -> return Nothing
  ValTerm vs    -> isInMaybeTupleType vs term env >>= \case
                      True  -> return (Just (map FValue vs))
                      _     -> return Nothing
  CompTerm _  _ -> return Nothing

-- to be used by seqMatcher
isInMaybeTermTypePreserve :: Values -> Maybe FTerm -> Env -> Rewrite (Maybe [Values])
isInMaybeTermTypePreserve v mty env = isInMaybeTermType v mty env >>= \case
  True  -> return (Just [v])
  _     -> return Nothing

isInMaybeTypeTypePreserve :: ComputationTypes -> Maybe FTerm -> Env -> Rewrite (Maybe [ComputationTypes])
isInMaybeTypeTypePreserve ty _ _ = return (Just [ty])

-- type checking
isInMaybeTermType :: Values -> (Maybe FTerm) -> Env -> Rewrite Bool
isInMaybeTermType v Nothing _ = return True
isInMaybeTermType v (Just term) env = 
    subsAndRewritesToValue term env >>= isIn v

-- | To type check a sequence values simply checker whether all elements
-- of the sequence are of the given type
isInMaybeTupleType :: [Values] -> FTerm -> Env -> Rewrite Bool
isInMaybeTupleType vs term env = 
  subsAndRewritesToValue term env >>= isInTuple vs . (:[])

isInTuple :: [Values] -> [Values] -> Rewrite Bool
isInTuple vs mtys = case sequence (map castType mtys) of
  Nothing  -> sortErr (FValue $ ADTVal "" (map FValue mtys)) 
                "rhs of annotation is not a type"
  Just tys -> Funcons.Patterns.isInTupleType vs (map paramFromType tys)

paramFromType (AnnotatedType ty op) = (ty, Just op)
paramFromType ty = (ty, Nothing)
 
isIn :: Values -> Values -> Rewrite Bool
isIn v mty = case castType mty of
    Nothing -> sortErr (FValue mty) "rhs of annotation is not a type"
    Just ty -> Funcons.Patterns.isInType v ty
  
isInType :: Values -> Types -> Rewrite Bool
isInType v (ADT "ground-values" []) = return (isGround v)
isInType v (ADT "maps" [kt', vt']) = case v of
  Map m -> do
    kt <- rewritesToType kt'
    vt <- rewritesToType vt'
    and <$> sequence 
          [and <$> mapM (flip Funcons.Patterns.isInType kt) (M.keys m)
          ,and <$> mapM (flip Funcons.Patterns.isInTupleType [paramFromType vt]) (M.elems m)]
  _ -> return False
isInType v (ADT "multisets" [ty']) = case v of
  Multiset ls -> do
    ty <- rewritesToType ty' 
    and <$> mapM (flip Funcons.Patterns.isInType ty) (toList ls)
  _ -> return False
isInType v (ADT "sets" [ty']) = case v of
  Set ls -> do
    ty <- rewritesToType ty' 
    and <$> mapM (flip Funcons.Patterns.isInType ty) (toList ls)
  _ -> return False
-- characters
isInType v AsciiCharacters = isInUnicodeType v "ascii-points"
isInType v ISOLatinCharacters = isInUnicodeType v "iso-latin-1-points"
isInType v BMPCharacters = isInUnicodeType v "basic-multilingual-plane-points"
-- type operators and datatypes
isInType v (ADT nm tys) = do
    DataTypeMemberss _ tpats alts <- typeEnvLookup nm
    or <$> mapM (isInAlt tpats) alts 
 where isInAlt tpats (DataTypeInclusionn ty_term) = do 
            -- TODO change DataTypeMember so that tpats are value-patterns
            --      requires change in the generator
            env <- fsMatch tys (map (PValue . VPType) tpats) emptyEnv
            subsAndRewritesToValue ty_term env >>= isIn v 
       isInAlt tpats (DataTypeMemberConstructor cons ty_terms mtpats)
        | ADTVal cons' args <- v, cons' == cons = do
          env <- case mtpats of 
                  Just tpats -> fsMatch tys (map (PValue . VPType) tpats) emptyEnv
                  Nothing    -> fsMatch tys (map (PValue . VPType) tpats) emptyEnv
          case all (not.hasStep) args of 
            True  -> mapM (flip subsAndRewritesToValues env) ty_terms >>= 
                        isInTuple (map downcastValue args) . concat
            _     -> return True -- imprecision
       isInAlt _ _ = return False
isInType v (AnnotatedType ty op) = Funcons.Patterns.isInTupleType [v] [(ty, Just op)] 
isInType v (Union ty1 ty2) = 
  (||) <$> Funcons.Patterns.isInType v ty1 <*> Funcons.Patterns.isInType v ty2
isInType v (Intersection ty1 ty2) = 
  (&&) <$> Funcons.Patterns.isInType v ty1 <*> Funcons.Patterns.isInType v ty2
isInType v (Complement t) = 
  not <$> Funcons.Patterns.isInType v t
isInType v t = maybe (return False) return (Funcons.Types.isInType v t)

isInUnicodeType v@(ADTVal c [p]) fname = do
  rangeTy   <- rewritesToType (applyFuncon fname [])
  isUnicode <- Funcons.Patterns.isInType v UnicodeCharacters
  point     <- rewritesToValue p
  inRange   <- Funcons.Patterns.isInType point rangeTy 
  return (and [isUnicode,inRange])
isInUnicodeType _ _ = return False


isInTupleType :: [Values] -> [TTParam] -> Rewrite Bool
isInTupleType vs ttparams = 
    eval_catch (vsMatch vs (map mkPattern ttparams) emptyEnv) >>= \case
        Right env' -> return True
        Left (_,_,PatternMismatch _) -> return False
        Left ie -> rewrite_rethrow ie 
    where mkPattern (ty, mop) = VPAnnotated ty_pat (TFuncon (type_ ty))
            where ty_pat = case mop of 
                                Nothing -> VPMetaVar "Dummy"
                                Just op -> VPSeqVar "Dummy" op

typeEnvLookup :: Name -> Rewrite DataTypeMembers 
typeEnvLookup con = Rewrite $ \ctxt st -> 
    case typeLookup con (ty_env ctxt) of
      Nothing -> (Left (evalctxt2exception(Internal ("type lookup failed: " ++ unpack con)) ctxt)
                        , st, mempty)
      Just members -> (Right members, st, mempty)

-- | 
-- Parameterisable evaluation function function for types.
rewriteType :: Name -> [Values] -> Rewrite Rewritten
rewriteType nm vs = rewritten (ComputationType(Type(ADT nm (map inject vs))))

pat2term :: FPattern -> FTerm
pat2term pat = case pat of
  PAnnotated pat _  -> pat2term pat
  PWildCard         -> TAny
  PMetaVar var      -> TVar var
  PSeqVar var _     -> TVar var
  PValue vpat       -> vpat2term vpat

vpat2term :: VPattern -> FTerm 
vpat2term vpat = case vpat of 
  PADT cons pats    -> case pats of [] -> TName cons
                                    _  -> TApp cons (map vpat2term pats)
  VPLit lit         -> TFuncon $ (FValue lit) 
  VPWildCard        -> TAny
--  PList pats        -> TList (map vpat2term pats)
  VPMetaVar var     -> TVar var
  VPSeqVar var _    -> TVar var
  VPAnnotated pat _ -> vpat2term pat
  VPType typat      -> typat2term typat

typat2term :: TPattern -> FTerm
typat2term typat = case typat of 
  TPADT cons pats     -> case pats of [] -> TName cons
                                      _  -> TApp cons (map typat2term pats)
  TPLit lit           -> lit 
  TPWildCard          -> TAny
  TPVar var           -> TVar var
  TPSeqVar var _      -> TVar var
  TPComputes f        -> TSortComputes (typat2term f)
  TPComputesFrom f t  -> TSortComputesFrom (typat2term f) (typat2term t)

