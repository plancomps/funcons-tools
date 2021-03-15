{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Entities (
    -- * Accessing entities
        -- ** mutables
        getMut, putMut, getMutPatt, putMutTerm,
        -- ** inherited
        getInh, withInh, getInhPatt, withInhTerm,
        -- ** control
        raiseSignal, receiveSignals, raiseTerm, receiveSignalPatt,
        withControlTerm, getControlPatt,
        -- ** output
        writeOut, readOut, writeOutTerm, readOutPatt,
        -- ** input
        matchInput, withExtraInput,withExactInput,
            withExtraInputTerms, withExactInputTerms,
    -- * Default entity values
        EntityDefaults, EntityDefault(..), setEntityDefaults
    )where

import Funcons.Types
import Funcons.MSOS
import Funcons.Substitution
import Funcons.Exceptions
import Funcons.Patterns

import Control.Applicative
import Control.Arrow
import qualified Data.Map as M
import Data.Text

-- defaults
-- | A list of 'EntityDefault's is used to declare (and possibly initialise)
-- entities.
type EntityDefaults = [EntityDefault]
-- | Default values of entities can be specified for /inherited/ 
-- and /mutable/ entities. 
data EntityDefault  = DefMutable Name Funcons   
                    | DefInherited Name Funcons 
                    -- | For the purpose of unit-testing it is advised to notify an interpreter of the existence of control, output and input entities as well.
                    | DefOutput Name
                    | DefControl Name
                    | DefInput Name

setEntityDefaults :: EntityDefaults -> MSOS StepRes -> MSOS StepRes
setEntityDefaults [] msos = msos
setEntityDefaults ((DefMutable nm f):rest) msos = 
    liftRewrite (rewriteFuncons f) >>= \case 
        ValTerm [v] -> putMut nm v >> setEntityDefaults rest msos
        ValTerm vs  -> liftRewrite $ exception f "default value evaluates to a sequence"
        _           -> liftRewrite $ exception f "default value requires steps to evaluate"
setEntityDefaults ((DefInherited nm f):rest) msos = 
    liftRewrite (rewriteFuncons f) >>= \case
        ValTerm vs  -> withInh nm vs (setEntityDefaults rest msos)
        _           -> liftRewrite $ exception f "default value requires steps to evaluate" 
setEntityDefaults ((DefControl nm):rest) msos = 
    withControl nm Nothing (setEntityDefaults rest msos) 
setEntityDefaults (_:rest) msos = setEntityDefaults rest msos

----------------------------------------------------
--- accessing entities

-- mutables

emptyMUT :: Mutable
emptyMUT = M.empty

giveMUT :: MSOS Mutable
giveMUT = MSOS $ \ctxt mut -> return (Right (mut_entities mut), mut, mempty)

-- | Get the value of some mutable entity.
getMut :: Name -> MSOS Values
getMut key = do  rw <- giveMUT
                 case M.lookup key rw of
                    Nothing -> return null__ 
                    Just v  -> return v

-- | Variant of 'getMut' that performs pattern-matching.
getMutPatt :: Name -> VPattern -> Env -> MSOS Env
getMutPatt nm pat env = do
    val <- getMut nm
    liftRewrite (vMatch val pat env)

modifyMUT :: Name -> (Values -> Values) -> MSOS ()
modifyMUT key f = do    rw <- giveMUT
                        newMUT (M.alter up key rw)
 where  up Nothing  = Just (f null__) 
        up (Just x) = Just (f x)

-- | Set the value of some mutable entity.
putMut :: Name -> Values -> MSOS ()
putMut key v = do rw <- giveMUT
                  newMUT (M.insert key v rw)

-- | Variant of 'putMut' that applies substitution.
putMutTerm :: Name -> FTerm -> Env -> MSOS ()
putMutTerm nm term env = liftRewrite (subsAndRewritesToValue term env) >>= putMut nm  

newMUT :: Mutable -> MSOS ()
newMUT rw = MSOS $ \ctxt mut-> return (Right(), mut {mut_entities = rw}, mempty)


-- input
-- | Consume a single value from the input stream.
-- | Throws an 'unsufficient input' exception, if not enough input is available.
consumeInput :: Name -> MSOS Funcons 
consumeInput nm = MSOS $ \ctxt mut ->
    case M.lookup nm (inp_es mut) of
      Just (vss, mreadM) -> wrapAttempt ctxt mut vss mreadM 
      Nothing            -> wrapAttempt ctxt mut [] (Just (def_fread ctxt nm))
  where
    wrapAttempt ctxt mut vss mreadM = case attemptConsume vss of
      Just (v,vss') -> return 
        (Right (FValue v), mut {inp_es = M.insert nm (vss',mreadM) (inp_es mut)},mempty)
      Nothing       -> case mreadM of
        Nothing -> return 
          (Left (ctxt2exception (InsufficientInput nm) ctxt), mut, mempty)
        Just readM -> do    v <- readM
                            return (Right v, mut, mempty)

    attemptConsume :: [[a]] -> Maybe (a,[[a]])
    attemptConsume []           = Nothing
    attemptConsume ((v:vs):vss) = Just (v,vs:vss)
    attemptConsume ([]:vss)     = second ([]:) <$> attemptConsume vss

-- | Provides /extra/ values to a certain input entity, available
-- to be consumed by the given 'MSOS' computation argument.
withExtraInput :: Name -> [Values] -> MSOS a -> MSOS a 
withExtraInput = withInput False

-- | Provides an /exact/ amount of input for some input entity, 
-- that is to be /completely/ consumed by the given 'MSOS' computation.
-- If less output is consumed a 'insufficient input consumed' exception
-- is thrown.
withExactInput :: Name -> [Values] -> MSOS a -> MSOS a
withExactInput = withInput True

withInput :: Bool -> Name -> [Values] -> MSOS a -> MSOS a
withInput isExactInput nm vs (MSOS f) = MSOS $ \ctxt mut -> do
  let provideInput newInp mreadM = do
        (a,mut',wr') <- f ctxt mut{ inp_es = M.insert nm newInp (inp_es mut)}
        let (res,vss'') = case (inp_es mut') M.! nm of
               ([]:vss',_) -> (a, vss')
               _           -> (Left(ctxt2exception(InsufficientInputConsumed nm) ctxt), vss'')
        return (res, mut' {inp_es = M.insert nm (vss'',mreadM) (inp_es mut')}, wr')
  case M.lookup nm (inp_es mut) of
    Just (vss, mreadM) -> 
      provideInput (vs:vss, if isExactInput then Nothing else mreadM) mreadM
    Nothing -> provideInput ([vs], Nothing) Nothing

-- | Variant of 'consumeInput' that matches the given `VPattern` to the consumed
-- value in the given 'Env'. 
matchInput :: Name -> VPattern -> Env -> MSOS Env
matchInput nm pat env = do 
    fs <- consumeInput nm
    vs <- liftRewrite (rewritesToValues fs)
    liftRewrite (vsMatch vs [pat] env)

-- | Variant of 'withExtraInput' that performs substitution.
withExtraInputTerms = withInputTerms False
-- | Variant of 'withExactInput' that performs substitution.
withExactInputTerms = withInputTerms True

withInputTerms :: Bool -> Name -> [FTerm] -> Env -> MSOS a -> MSOS a
withInputTerms b nm fs env msos = do
    vs <- liftRewrite (mapM (flip subsAndRewritesToValue env) fs)
    withInput b nm vs msos

-- control
-- | Receive the value of a control entity from a given 'MSOS' computation.
receiveSignals :: [Name] -> MSOS a -> MSOS (a, [Maybe Values])
receiveSignals keys (MSOS f) = MSOS (\ctxt mut -> do
    (e_a, mut1, wr1) <- f ctxt mut
    case e_a of 
        Left err -> return (Left err, mut1, wr1)
        Right a  -> return $ 
          (Right (a, Prelude.map (find (ctrl_entities wr1)) keys)
                 , mut1, wr1 {ctrl_entities = Prelude.foldr M.delete (ctrl_entities wr1) keys}))
  where find m key = maybe Nothing id $ M.lookup key m

-- | Variant of 'receiveSignal' that performs pattern-matching.
receiveSignalPatt :: Maybe Values -> Maybe VPattern -> Env -> MSOS Env
receiveSignalPatt mval mpat env = liftRewrite (vMaybeMatch mval mpat env)

-- | Signal a value of some control entity.
raiseSignal :: Name -> Values -> MSOS ()
raiseSignal nm v = MSOS (\ctxt mut -> return 
                        (Right (), mut, mempty { ctrl_entities = singleCTRL nm v}))

-- | Variant of 'raiseSignal' that applies substitution.
raiseTerm :: Name -> FTerm -> Env -> MSOS ()
raiseTerm nm term env = liftRewrite (subsAndRewritesToValue term env) >>= raiseSignal nm 

-- downwards control
-- | Set the value of an downwards control entity. 
-- The new value is /only/ set for 'MSOS' computation given as a third argument.
withControl :: Name -> Maybe Values -> MSOS a -> MSOS a
withControl key mfct (MSOS f) = MSOS (\ctxt mut  -> 
    let ctxt' = ctxt { dctrl_entities = M.insert key mfct (dctrl_entities ctxt) } 
    in f ctxt' mut)

withControlTerm :: Name -> Maybe FTerm -> Env -> MSOS a -> MSOS a
withControlTerm nm mterm env msos = do
    mfct <- case mterm of 
              Nothing   -> return Nothing
              Just term -> Just <$> liftRewrite (substitute_signal term env)
    withControl nm mfct msos

-- | Get the value of an down control entity.
getControl :: Name -> MSOS (Maybe Values)
getControl key = do  
  ro <- giveCTRL
  case M.lookup key ro of
    Nothing   -> return Nothing
    Just mv   -> return mv

-- | Version of 'getControl' that applies pattern-matching.
getControlPatt :: Name -> Maybe VPattern -> Env -> MSOS Env
getControlPatt nm mpat env = do
    mpat' <- liftRewrite $ maybe (return Nothing) (fmap Just . flip substitute_patt_signal env) mpat
    mfct <- getControl nm
    liftRewrite (eval_catch (vMaybeMatch mfct mpat' env) >>= \case
      Left (_,_,PatternMismatch _)  -> return env --TODO suboptimal 
      Left exc                      -> rewrite_rethrow exc
      Right env'                    -> return env')


-- inherited

-- | Get the value of an inherited entity.
getInh :: Name -> MSOS [Values]
getInh key = do  ro <- giveINH
                 case M.lookup key ro of
                    Nothing -> return [null__] 
                    Just vs -> return vs

-- | Version of 'getInh' that applies pattern-matching.
getInhPatt :: Name -> [VPattern] -> Env -> MSOS Env
getInhPatt nm pats env = do
    vals <- getInh nm
    liftRewrite (vsMatch vals pats env)

-- | Set the value of an inherited entity. 
-- The new value is /only/ set for 'MSOS' computation given as a third argument.
withInh :: Name -> [Values] -> MSOS a -> MSOS a
withInh key v (MSOS f) = MSOS (\ctxt mut  -> 
        let ctxt' = ctxt { inh_entities = M.insert key v (inh_entities ctxt) } 
        in f ctxt' mut)

-- | Variant of 'withInh' that performs substitution.
withInhTerm :: Name -> FTerm -> Env -> MSOS a -> MSOS a
withInhTerm nm term env msos = do
    v <- liftRewrite $ (subsAndRewritesToValues term env)
    withInh nm v msos

-- output
-- | Add new values to a certain output entity.
writeOut :: Name -> [Values] -> MSOS ()
writeOut key vs = MSOS $ \ctxt mut -> return (Right (), mut
                            ,mempty { out_entities = M.singleton key vs })

-- | Variant of 'writeOut' that applies substitution.
writeOutTerm :: Name -> FTerm -> Env -> MSOS ()
writeOutTerm nm term env = 
  liftRewrite (subsAndRewritesToValues term env) >>= writeOut nm

-- | Read the values of a certain output entity. The output is obtained
-- from the 'MSOS' computation given as a second argument.
readOut :: Name -> MSOS a -> MSOS (a,[Values])
readOut key msos = readOuts msos >>= 
                        return . fmap (maybe [] id . M.lookup key)

-- | Variant of 'readOut' that performs pattern-matching.
readOutPatt :: Name -> VPattern -> MSOS Env -> MSOS Env 
readOutPatt key pat msos = do
    (env, vals) <- readOut key msos
    liftRewrite (vMatch (ADTVal "list" (Prelude.map FValue vals)) pat env)
