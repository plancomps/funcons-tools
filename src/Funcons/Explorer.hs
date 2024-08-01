{-# LANGUAGE FlexibleInstances, OverloadedStrings, LambdaCase, FlexibleContexts, RankNTypes, MultiParamTypeClasses #-}

module Funcons.Explorer where

import qualified Language.Explorer.Monadic as EI

import Funcons.EDSL hiding (isMap)
import Funcons.Operations (isMap, Values(Map), EvalResult(..)) 
import Funcons.MSOS
import Funcons.RunOptions
import Funcons.Core
import Funcons.Core.Library
import Funcons.Core.Manual
import Funcons.Entities
import Funcons.Tools
import Funcons.Parser
import Funcons.Printer
import Funcons.Exceptions
import Funcons.Types (Funcons(..))

import Control.Monad (forM_, mapM_, join)
import Control.Monad.Trans.Class (lift) 
import Data.IORef
import qualified Data.Map as M
import Data.Char (isSpace)
import Data.Tree (drawTree)
import Data.Text (pack)
import Text.Read (readMaybe)
import Data.Maybe (fromJust, isJust)

import System.Console.Haskeline
import System.Console.Haskeline.History
import System.Environment
import System.IO
import System.IO.Unsafe

import qualified MVD.Interface as MVD
import qualified MVD.Debugger as MVD
import qualified MVD.Finders as MVD


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

repll :: IO ()
repll = display_help >> getArgs >>= mk_explorer >>= (runInputT defaultSettings . repl')
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


mk_interpreter :: [String] -> IO (RunOptions, Config)
mk_interpreter args = do 
  (opts, unknown_opts) <- run_options args
  forM_ unknown_opts $ \arg -> do
      putStrLn ("unknown option: " ++ arg) 
  opts_ref <- newIORef opts 
  cfg <- mk_initial_config library entities typeenv opts
  return (opts, cfg)
  where 
    library = libUnions [ Funcons.Core.Library.funcons, Funcons.EDSL.library, Funcons.Core.Manual.library ]
    entities = Funcons.Core.Library.entities 
    typeenv = Funcons.Core.Library.types


 
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
              msos_ctxt ((emptyMSOSState (random_seed opts)) {inp_es = M.empty})) (inputValues opts)
  return $ Config { reader = init msos_ctxt, state = mut, progress = done }
  where f0 = initialise_binding_ [initialise_storing_ [map_empty_ []]]
        init msos_reader = msos_reader {inh_entities = M.insert "environment" [Map M.empty] (inh_entities msos_reader) }

def_interpreter :: IORef RunOptions -> Phrase -> Config -> IO (Maybe Config)
def_interpreter opts_ref phrase cfg = do 
  opts <- readIORef opts_ref 
  case phrase of FTerm f0' -> let f0 = prep_term f0'
                              in fmap (setProgress done) <$> exec opts (loop opts) f0 (prep_ctxt f0 opts) []
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
                                Left fct  -> fmap (setProgress done) <$> exec opts (loop opts) fct (prep_ctxt fct opts) [] 
                                Right vs  -> putStrLn "already done.." >> return (Just cfg)
 where prep_term f0' =
        give_ [f0', 
          give_ [if_else_ [is_ [given_, environments_], given_
                          ,if_else_ [is_ [given_, null_type_], given_
                                    ,bind_ [Funcons.EDSL.string_ "it", given_]]]
                ,if_else_ [is_ [given_, null_type_], given_
                          ,sequential_ [print_ [given_,Funcons.EDSL.string_ "\n"], given_]]]]
       prep_ctxt f0 opts = (reader cfg) { ereader = (ereader (reader cfg)) { local_fct = f0, global_fct = f0, run_opts = opts } }
       mk_step opts f0 = do im <- exec opts step f0 (prep_ctxt f0 opts) []
                            case im of Just cfg -> case progress cfg of 
                                                     Left fct -> putStrLn ("\nremaining funcon term:\n" ++ ppFuncons opts fct)
                                                     _        -> return ()
                                       Nothing -> return ()
                            return im

       exec opts stepper f0 msos_ctxt nd_choices = do 
        (e_exc_f, mut, wr) <- runMSOS (stepper f0) msos_ctxt (setNDs nd_choices $ state cfg)
        case e_exc_f of
          Left (_,local,NDEncounter ndsrc) -> do
            putStrLn (ndtype ++ " non-determinism encountered in: " ++ ppFuncons opts local)
            nd_choice <- runInputT defaultSettings (nd_selection opts ndsrc)
            exec opts stepper f0 msos_ctxt (nd_choices++[nd_choice])
           where ndtype = case ndsrc of NDInputInterleaving _ -> "interleaving"
                                        NDInputValueOperations _ -> "value-operation"
                                        NDInputPattern _ -> "pattern" 
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

setNDs :: [Int] -> MSOSState m -> MSOSState m
setNDs is ctxt = ctxt { estate = (estate ctxt) { nd_choice = is } }

-- assumes all components of RewriteReader do not change per session
instance Eq (MSOSReader IO) where
  r1 == r2 = inh_entities r1 == inh_entities r2 
          && dctrl_entities r1 == dctrl_entities r2

-- assumes input is not used // does not change per session
instance Eq (MSOSState IO) where
  s1 == s2 = mut_entities s1 == mut_entities s2 

nd_selection :: RunOptions -> NDInput -> InputT IO Int
nd_selection opts ndsrc = do
  lift $ putStrLn "choose from the following alternatives:"
  lift $ forM_ (zip [1..] alts) (\(i,str) -> 
    putStrLn (show i ++ ") " ++ str)
    )
  mint <- getInputLine ("by selecting a number between " ++ show 1 ++ " and " ++ show m ++ "\n>")
  case join (fmap readMaybe mint) of
    Just i | i >= 1 && i <= m -> return (i-1)
    otherwise                 -> nd_selection opts ndsrc
  where m = length alts
        alts = case ndsrc of
          NDInputValueOperations eress -> concatMap display eress
            where display (Error _ _)         = []
                  display (EvalResults eress) = concatMap display eress
                  display (Success fct)       = [ppFuncons opts fct]
          NDInputInterleaving fcts -> map (ppFuncons opts) fcts
          NDInputPattern iis -> map toStr iis
            where toStr (k, r) = "variable #" ++ show (k+1) ++ " matching " ++ show r ++ " values"

display_environment :: Config -> IO ()
display_environment cfg =
  mapM_ (putStrLn . showValues) (inh_entities (reader cfg) M.! "environment")

display_mut_entity :: Config -> String -> IO ()
display_mut_entity cfg ent = 
  case M.lookup (pack ent) (mut_entities (state cfg)) of 
    Nothing -> putStrLn ("unknown mutable entity: " ++ ent)
    Just v  -> putStrLn $ showL (map showValues v)

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


type DebugConfig = DFunconsConfig

instance MVD.Reduce () DebugConfig DebugConfig where 
    rstate _ c = c

instance MVD.Evaluate DebugConfig DebugConfig Bool where 
    estate goal curr = goal == curr 

instance Show Config where 
  show c = show (progress c)



repl :: IO ()
repl = getArgs >>= mk_interpreter >>= (runInputT defaultSettings . buildDebugger)
  where buildDebugger (runopts, cfg) = do
          getInputLine " > " >>= \case
            Nothing -> return ()
            (Just input) -> do
                case fct_parse_either input of 
                  Left err  -> outputStrLn err
                  Right fct -> lift $ MVD.debugger (printDFunconsConfig runopts) (putStrLn . showFunconsActions runopts) (funconsSTR runopts (debugExecute runopts) (cfg { progress = Left fct})) MVD.equalityFinder (DFunconsConfig { nconfig = cfg, ndeter = Nothing }) ()


data FunconsActions = FStep | NDChoice Int NDInput


showFunconsActions :: RunOptions -> FunconsActions -> String
showFunconsActions _ FStep = "Step"
showFunconsActions opts (NDChoice i (NDInputInterleaving [f])) = 
  "Choose: " ++ ppFuncons opts f
showFunconsActions opts (NDChoice i (NDInputValueOperations [s])) = 
  "Pick value: " ++ display s
  where display (Error _ _)         = ""
        display (EvalResults eress) = concatMap display eress
        display (Success fct)       = join [ppFuncons opts fct]
showFunconsActions opts (NDChoice i (NDInputPattern k)) = 
  concatMap toStr k
  where toStr (k, r) = "variable #" ++ show (k+1) ++ " matching " ++ show r ++ " values"

data DFunconsConfig = DFunconsConfig { nconfig :: Config, ndeter :: Maybe (Funcons, NDInput), ndchoice :: [Int], opts :: RunOptions }

printDFunconsConfig :: RunOptions -> DFunconsConfig -> IO ()
printDFunconsConfig opts c 
  | isJust (ndeter c) = do
    putStr "Current term: "
    putStrLn $ showProgress opts (progress $ nconfig c)
    putStrLn $ "Non-determinism choice at: " ++ ppFuncons opts (fst . fromJust . ndeter $ c)
  | otherwise =  do
    putStr "Current term: "
    putStrLn $ showProgress opts (progress $ nconfig c)


instance Eq DFunconsConfig where 
  d1 == d2 = nconfig d1 == nconfig d2

-- Funcons [Values]
showProgress :: RunOptions -> StepRes -> String
showProgress opts (Left f) = ppFuncons opts f
showProgress _ (Right [vs]) = show vs
showProgress _ (Right vs) = show vs

instance Show DFunconsConfig where 
  show c = case ndeter c of 
    Nothing -> (showProgress (opts c) . progress $ nconfig c) ++ "\n" 
    (Just (l, _)) -> show (nconfig c) ++ "\n" ++ "Has non-determinism: " ++ ppFuncons (opts c) l ++ "\n"


debugExecute :: RunOptions -> DFunconsConfig -> IO [DFunconsConfig]
debugExecute opts dcfg = do 
  case progress cfg of 
      Left fct  -> mk_step opts fct
      Right vs  -> return [dcfg]
  where 
        cfg = nconfig dcfg
        mk_step opts f0 = exec opts (loop opts) f0 (prep_ctxt f0 opts) (ndchoice dcfg)
        
        prep_ctxt :: Funcons -> RunOptions -> MSOSReader IO
        prep_ctxt f0 opts = (reader cfg) { ereader = (ereader (reader cfg)) { local_fct = f0, global_fct = f0, run_opts = opts } }

        exec :: t -> (t1 -> MSOS (Either Funcons [Funcons.Operations.Values Funcons])) -> t1 -> MSOSReader IO -> [Int] -> IO [DFunconsConfig]
        exec opts stepper f0 msos_ctxt nd_choices = do 
          (e_exc_f, mut, wr) <- runMSOS (stepper f0) msos_ctxt (setNDs nd_choices $ state cfg)
          case e_exc_f of
            Left (curr, local, NDEncounter ndsrc) -> return [dcfg {ndeter = Just (local, ndsrc), nconfig = (nconfig dcfg) { progress = Left curr}} ]
            Left ie    -> putStrLn (showIException ie) >> return []
            Right (Left fct) -> return $ [dcfg { nconfig = cfg { state = mut, progress = Left fct}}] -- did not yield an environment
            Right (Right efvs) -> case filter isMap efvs of
              []    -> return $ [dcfg { nconfig = cfg { state = mut, progress = Right efvs } }]
              [env] -> return $ [dcfg { nconfig = cfg { reader = accumulate (reader cfg) env, state = mut, progress = Right efvs } } ]
              _     -> return [] 
          where accumulate msos_reader env = msos_reader { inh_entities = M.update override "environment" (inh_entities msos_reader) }
                  where override [old_env] = case (env, old_env) of 
                          (Map m1, Map m2) -> Just [Map (M.union m1 m2)] 
                          _                -> Nothing
                        override _ = Nothing


debugActions :: DFunconsConfig -> [FunconsActions]
debugActions c = case ndeter c of 
  Nothing -> [FStep]
  (Just (local, NDInputInterleaving l)) -> [NDChoice i (NDInputInterleaving [(l !! i)]) | i <- [0..length l - 1]]
  (Just (local, NDInputPattern l)) ->  [NDChoice i (NDInputPattern [(l !! i)]) | i <- [0..length l - 1]]
  (Just (local, NDInputValueOperations l)) -> [NDChoice i (NDInputValueOperations [(l !! i)]) | i <- [0..length l - 1]]


debugExecute' interp c p = 
  case p of 
    FStep -> unsafePerformIO $ interp c
    (NDChoice i _) -> unsafePerformIO $ interp (c { ndchoice = ndchoice c ++ [i], ndeter = Nothing })

funconsSTR :: RunOptions -> (DFunconsConfig -> IO [DFunconsConfig]) -> Config -> MVD.STR DFunconsConfig FunconsActions
funconsSTR ropts interp c = MVD.STR 
  { MVD.initial = [DFunconsConfig { nconfig = c, ndeter = Nothing, ndchoice = [], opts = ropts }]
  , MVD.actions = debugActions
  , MVD.execute = debugExecute' interp
  }