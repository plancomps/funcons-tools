{-# LANGUAGE FlexibleInstances, OverloadedStrings, LambdaCase #-}

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

import Control.Monad (forM_, mapM_)
import Data.IORef
import qualified Data.Map as M
import Data.Char (isSpace)
import Data.Tree (drawTree)
import Data.Text (pack)
import Text.Read (readMaybe)

import Control.Monad.Trans.Class (lift) 
import System.Console.Haskeline
import System.Console.Haskeline.History
import System.Environment
import System.IO

data Phrase = FTerm Funcons
            | Debug Funcons
            | Step 
            | SmallStep 
            | PrettyBigStep 
            | Finish
      deriving (Show, Eq) 

data Config = Config {
        reader  :: MSOSReader IO
      , state   :: MSOSState IO
      , progress:: StepRes 
      }
      deriving (Eq)

type Explorer = EI.Explorer Phrase IO Config ()

handle_revert :: EI.Ref -> Explorer -> IO Explorer
handle_revert r exp =
  case EI.revert r exp of
    Just e -> return e
    Nothing -> putStrLn "Invalid reference for revert" >> return exp

repl :: IO ()
repl = display_help >> getArgs >>= mk_explorer >>= (runInputT defaultSettings . repl')
 where 
  repl' exp = do
   getInputLine ("#" ++ show (EI.currRef exp) ++ " > ") >>= \case 
    Nothing    -> return ()
    Just input -> do
      case break isSpace input of
        (":help",_)       -> lift display_help >> repl' exp
        (":h",_)          -> lift display_help >> repl' exp
        (":quit",_)       -> return ()
        (":q",_)          -> return ()
        (":env",_)        -> lift (display_environment (EI.config exp)) >> repl' exp 
        (":environment",_)-> lift (display_environment (EI.config exp)) >> repl' exp
        (":store",_)      -> lift (display_mut_entity (EI.config exp) "store") >> repl' exp
        (":sto",_)        -> lift (display_mut_entity (EI.config exp) "store") >> repl' exp
        (":mut", rest)    -> lift (display_mut_entity (EI.config exp) (dropWhile isSpace rest)) >> repl' exp 
        (":mutable", rest)-> lift (display_mut_entity (EI.config exp) (dropWhile isSpace rest)) >> repl' exp 
        (":session", _)   -> do
          (outputStrLn . drawTree . fmap (show . fst) . EI.toTree) exp
          repl' exp
        (":revert", mint) | Just ref_id' <- readMaybe (dropWhile isSpace mint)
                          -> lift (handle_revert ref_id' exp)  >>= repl'
                          | otherwise -> outputStrLn "Revert requires an integer argument" >> repl' exp
        (":debug", mfct)  -> case fct_parse_either (dropWhile isSpace mfct) of
                              Left err -> outputStrLn err >> repl' exp
                              Right fct -> lift (EI.execute (Debug fct) exp) >>= (repl' . fst)
        (":step", _)      -> lift (EI.execute Step exp) >>= (repl' . fst)
        (":small-step", _)-> lift (EI.execute SmallStep exp) >>= (repl' . fst)
        (":pretty-big-step", _)      -> lift (EI.execute PrettyBigStep exp) >>= (repl' . fst)
        (":finish", _)    -> lift (EI.execute Finish exp) >>= (repl' . fst)
        _                 -> case fct_parse_either input of 
                                 Left err  -> outputStrLn err >> repl' exp 
                                 Right fct -> lift (EI.execute (FTerm fct) exp) >>= (repl'  . fst)

  
mk_explorer :: [String] -> IO Explorer 
mk_explorer args = do
  (opts, unknown_opts) <- run_options args
  forM_ unknown_opts $ \arg -> do
      putStrLn ("unknown option: " ++ arg) 
  opts_ref <- newIORef opts 
  cfg <- mk_initial_config library entities typeenv opts
  return $ EI.mkExplorer False (const . const $ False) (\f c -> (\c -> (c, ())) <$> def_interpreter opts_ref f c) cfg
 where
  library = libUnions [ Funcons.Core.Library.funcons, Funcons.EDSL.library, Funcons.Core.Manual.library ]
  entities = Funcons.Core.Library.entities 
  typeenv = Funcons.Core.Library.types

mk_initial_config :: FunconLibrary -> EntityDefaults -> TypeRelation -> RunOptions -> IO Config
mk_initial_config lib defaults tyenv opts = do
  let msos_ctxt = MSOSReader (RewriteReader lib tyenv opts f0 f0) emptyINH emptyDCTRL (fread (string_inputs opts))
  ((e_exc_f, mut, wr), rem_ins) <- 
      fexec (runMSOS (setEntityDefaults defaults (loop opts f0))
              msos_ctxt (emptyMSOSState {inp_es = M.empty})) (inputValues opts)
  return $ Config { reader = init msos_ctxt, state = mut, progress = done }
  where f0 = initialise_binding_ [initialise_storing_ [map_empty_ []]]
        init msos_reader = msos_reader {inh_entities = M.insert "environment" [Map M.empty] (inh_entities msos_reader) }

def_interpreter :: IORef RunOptions -> Phrase -> Config -> IO (Maybe Config)
def_interpreter opts_ref phrase cfg = do 
  opts <- readIORef opts_ref 
  case phrase of FTerm f0' -> let f0 = prep_term f0'
                              in fmap (setProgress done) <$> exec (loop opts) f0 (prep_ctxt f0 opts)
                 Debug f0' -> let f0 = prep_term f0'
                              in putStrLn ("\nremaining funcon term:\n" ++ ppFuncons opts f0) 
                              >> return (Just (cfg { progress = Left f0 }))
                 Step      -> case progress cfg of 
                                Left fct  -> mk_step opts fct
                                Right vs  -> putStrLn "already done.." >> return (Just cfg)
                 SmallStep -> case progress cfg of 
                                Left fct  -> mk_step (turn_off_refocus opts) fct
                                Right vs  -> putStrLn "already done.." >> return (Just cfg)
                 PrettyBigStep -> case progress cfg of 
                                Left fct  -> mk_step (turn_on_refocus opts) fct
                                Right vs  -> putStrLn "already done.." >> return (Just cfg)
                 Finish    -> case progress cfg of
                                Left fct  -> fmap (setProgress done) <$> exec (loop opts) fct (prep_ctxt fct opts)
                                Right vs  -> putStrLn "already done.." >> return (Just cfg)
 where prep_term f0' =
        give_ [f0', 
          give_ [if_else_ [is_ [given_, environments_], given_
                          ,if_else_ [is_ [given_, null_type_], given_
                                    ,bind_ [Funcons.EDSL.string_ "it", given_]]]
                ,if_else_ [is_ [given_, null_type_], given_
                          ,sequential_ [print_ [given_,Funcons.EDSL.string_ "\n"], given_]]]]
       prep_ctxt f0 opts = (reader cfg) { ereader = (ereader (reader cfg)) { local_fct = f0, global_fct = f0, run_opts = opts } }
       mk_step opts f0 = do im <- exec step f0 (prep_ctxt f0 opts)
                            case im of Just cfg -> case progress cfg of 
                                                     Left fct -> putStrLn ("\nremaining funcon term:\n" ++ ppFuncons opts fct)
                                                     _        -> return ()
                                       Nothing -> return ()
                            return im

       exec stepper f0 msos_ctxt = do 
        (e_exc_f, mut, wr) <- runMSOS (stepper f0) msos_ctxt (state cfg)
        case e_exc_f of
          Left ie    -> putStrLn (showIException ie) >> return Nothing 
          Right (Left fct) -> return $ Just $ cfg { state = mut, progress = Left fct} -- did not yield an environment
          Right (Right efvs) -> case filter isMap efvs of
            []    -> return $ Just $ cfg { state = mut, progress = Right efvs }
            [env] -> return $ Just $ cfg { reader = accumulate (reader cfg) env, state = mut, progress = Right efvs } 
            _     -> putStrLn ("multiple environments computed") >> return Nothing
        where accumulate msos_reader env = msos_reader { inh_entities = M.update override "environment" (inh_entities msos_reader) }
                where override [old_env] = case (env, old_env) of 
                        (Map m1, Map m2) -> Just [Map (M.union m1 m2)] 
                        _                -> Nothing
                      override _ = Nothing

loop :: RunOptions -> Funcons -> MSOS StepRes
loop opts = stepTrans opts 0 . toStepRes

step :: Funcons -> MSOS StepRes
step = stepAndOutput

done :: StepRes
done = Right []

setProgress :: StepRes -> Config -> Config
setProgress res cfg = cfg { progress = res }

-- assumes all components of RewriteReader do not change per session
instance Eq (MSOSReader IO) where
  r1 == r2 = inh_entities r1 == inh_entities r2 
          && dctrl_entities r1 == dctrl_entities r2

-- assumes input is not used // does not change per session
instance Eq (MSOSState IO) where
  s1 == s2 = mut_entities s1 == mut_entities s2 

display_environment :: Config -> IO ()
display_environment cfg =
  mapM_ (putStrLn . showValues) (inh_entities (reader cfg) M.! "environment")

display_mut_entity :: Config -> String -> IO ()
display_mut_entity cfg ent = 
  case M.lookup (pack ent) (mut_entities (state cfg)) of 
    Nothing -> putStrLn ("unknown mutable entity: " ++ ent)
    Just v  -> putStrLn $ showValues v

display_help :: IO ()
display_help =
  putStrLn  "Available commands:\n\
            \  :environment :env    show the active bindings from identifiers to values\n\
            \  :store :sto          show the store with assignments to references\n\
            \  :mutable :mut <ENT>  show the mutable entity with name <ENT>\n\
            \  :session             displays the explored traces in the form of a tree\n\
            \                       with nodes labelled by state identifiers\n\
            \  :revert <INT>        revert to the state with id <INT>\n\
            \  :debug <FCT>         start step-by-step execution of funcon term <FCT>\n\
            \  :step                perform the next step in a step-by-step execution (without changing the refocusing setting)\n\
            \  :pretty-big-step     perform the next step in a step-by-step execution with refocusing\n\
            \  :small-step          perform the next step in a step-by-step execution without refocusing\n\
            \  :finish              perform all remaining steps of a step-by-step execution\n\
            \  :help :h             show these commands\n\
            \  :quit :q             end the exploration\n\
            \  or just type a funcon term"
