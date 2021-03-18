{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Funcons.Explorer where

import qualified Language.Explorer.Monadic as EI

import Funcons.EDSL hiding (isMap)
import Funcons.Operations (isMap, Values(Map)) 
import Funcons.MSOS
import Funcons.RunOptions
import Funcons.Core
import Funcons.Core.Library
import Funcons.Core.Manual
import Funcons.Entities
import Funcons.Tools
import Funcons.Parser
import Funcons.Printer

import Control.Monad (forM_)
import Data.IORef
import qualified Data.Map as M
import Data.Char (isSpace)
import Data.Tree (drawTree)
import Text.Read (readMaybe)

import System.Environment
import System.IO

data Config = Config {
        reader  :: MSOSReader IO
      , state   :: MSOSState IO 
      }
      deriving (Eq)

type Explorer = EI.Explorer Funcons IO Config ()

handle_revert :: EI.Ref -> Explorer -> IO Explorer
handle_revert r exp =
  case EI.revert r exp of
    Just e -> return e
    Nothing -> putStrLn "Invalid reference for revert" >> return exp
    

repl :: IO ()
repl = getArgs >>= mk_explorer >>= repl'
 where 
  repl' exp = do
    putStr ("#" ++ show (EI.currRef exp) ++ " > ") >> hFlush stdout
    input <- getLine
    case break isSpace input of
      (":session", _)   -> do
        (putStrLn . drawTree . fmap (show . fst) . EI.toTree) exp
        repl' exp
      (":revert", mint) | Just ref_id' <- readMaybe (dropWhile isSpace mint)
                        -> handle_revert ref_id' exp  >>= repl'
                        | otherwise -> putStrLn "Revert requires an integer argument" >> repl' exp
      _                 -> case fct_parse_either input of 
                               Left err  -> putStrLn err >> repl' exp 
                               Right fct -> EI.execute fct exp >>= (repl'  . fst)

  
mk_explorer :: [String] -> IO Explorer 
mk_explorer args = do
  (opts, unknown_opts) <- run_options args
  forM_ unknown_opts $ \arg -> do
      putStrLn ("unknown option: " ++ arg) 
  opts_ref <- newIORef opts 
  cfg <- mk_initial_config library entities typeenv opts
  return $ EI.mkExplorerTree (\f c -> (\c -> (c, ())) <$> def_interpreter opts_ref f c) cfg
 where
  library = libUnions [ Funcons.Core.Library.funcons, Funcons.EDSL.library, Funcons.Core.Manual.library ]
  entities = Funcons.Core.Library.entities 
  typeenv = Funcons.Core.Library.types

mk_initial_config :: FunconLibrary -> EntityDefaults -> TypeRelation -> RunOptions -> IO Config
mk_initial_config lib defaults tyenv opts = do
  let msos_ctxt = MSOSReader (RewriteReader lib tyenv opts f0 f0) emptyINH emptyDCTRL (fread (string_inputs opts))
  ((e_exc_f, mut, wr), rem_ins) <- 
      fexec (runMSOS (setEntityDefaults defaults (stepTrans opts 0 (toStepRes f0)))
              msos_ctxt (emptyMSOSState {inp_es = M.empty})) (inputValues opts)
  return $ Config { reader = init msos_ctxt, state = mut }
  where f0 = initialise_binding_ [initialise_storing_ [map_empty_ []]]
        init msos_reader = msos_reader {inh_entities = M.insert "environment" [Map M.empty] (inh_entities msos_reader) }

def_interpreter :: IORef RunOptions -> Funcons -> Config -> IO (Maybe Config)
def_interpreter opts_ref f0 cfg = do
  opts <- readIORef opts_ref
  let msos_ctxt = (reader cfg) { ereader = (ereader (reader cfg)) { local_fct = f0, global_fct = f0 } }
  (e_exc_f, mut, wr) <- runMSOS (stepTrans opts 0 (toStepRes f0)) msos_ctxt (state cfg)
  case e_exc_f of
    Left ie    -> putStrLn (showIException ie) >> return Nothing 
    Right (Left fct) -> return $ Just $ cfg { state = mut } -- did not yield an environment
    Right (Right efvs) -> case filter isMap efvs of
      []    -> return $ Just $ cfg { state = mut }
      [env] -> return $ Just $ cfg { reader = accumulate (reader cfg) env, state = mut } 
      _     -> putStrLn ("multiple environments computed") >> return Nothing
  where accumulate msos_reader env = msos_reader { inh_entities = M.update override "environment" (inh_entities msos_reader) }
          where override [old_env] = case (env, old_env) of 
                  (Map m1, Map m2) -> Just [Map (M.union m1 m2)] 
                  _                -> Nothing
                override _ = Nothing

-- assumes all components of RewriteReader do not change per session
instance Eq (MSOSReader IO) where
  r1 == r2 = inh_entities r1 == inh_entities r2 
          && dctrl_entities r1 == dctrl_entities r2

-- assumes input is not used // does not change per session
instance Eq (MSOSState IO) where
  s1 == s2 = mut_entities s1 == mut_entities s2 
