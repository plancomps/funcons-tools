{-# LANGUAGE OverloadedStrings #-}

module Funcons.Tools (
    -- * Creating standalone interpreters.

    -- $moduledoc
    mkMain, mkMainWithLibrary, mkMainWithLibraryEntities,
    mkMainWithLibraryTypes, mkMainWithLibraryEntitiesTypes,
    mkFullyFreshInterpreter, mkFreshInterpreter,
    -- * Creating embedded interpreters.
    run, runWithExtensions, runWithExtensionsNoCore, runWithExtensionsNoNothing,
    -- * Utility functions for interpreter extensions. 
    -- ** Funcon libraries.
    FunconLibrary, libEmpty, libUnion, libOverride, libUnions, libOverrides, libFromList,
    -- ** Type environments.
    TypeRelation, DataTypeMembers(..), DataTypeAltt(..), TypeParam, 
        emptyTypeRelation, typeEnvUnion, typeEnvUnions, typeEnvFromList,
    -- ** Entity declarations 
    EntityDefaults, EntityDefault(..), noEntityDefaults,
    ) where

import Funcons.EDSL (library)
import Funcons.RunOptions
import Funcons.Types
import Funcons.Entities
import Funcons.MSOS
import Funcons.Core.Library
import Funcons.Core.Manual
import Funcons.Printer
import Funcons.Parser

import System.Environment (getArgs)

import Data.Text (unpack)
import Data.List ((\\), intercalate)
import qualified Data.Map as M
import Control.Monad (forM_, when, unless,join)

-- | The empty collection of entity defaults.
noEntityDefaults :: [EntityDefault]
noEntityDefaults = []

-- | Creates a /main/ function for the default interpreter (no extension).
-- The executable made from this /main/ function receives command line
-- argumenst as explained above ("Funcons.Tools"). 
mkMain :: IO ()
mkMain = mkMainWithLibrary libEmpty 

-- | Creates a /main/ function for the interpreter obtained by extending
-- the default library with the funcons in the 'FunconLibrary' argument.
mkMainWithLibrary :: FunconLibrary -> IO() 
mkMainWithLibrary lib = mkMainWithLibraryEntities lib [] 

-- | Creates a /main/ function for the interpreter obtained by extending
-- the main interpreter  with the funcons in the 'FunconLibrary' argument
-- and with default values for entities defined in the 'EntityDefaults' 
-- argument.
mkMainWithLibraryEntities :: FunconLibrary -> EntityDefaults -> IO ()
mkMainWithLibraryEntities lib ents = 
    mkMainWithLibraryEntitiesTypes lib ents emptyTypeRelation

-- | Creates a /main/ function for the interpreter obtained by extending
-- the main interpreter with the funcons in the 'FunconLibrary' argument
-- and with a 'TypeRelation' mapping datatypes to their constructors and
-- type arguments.
mkMainWithLibraryTypes :: FunconLibrary -> TypeRelation -> IO ()
mkMainWithLibraryTypes lib tys = mkMainWithLibraryEntitiesTypes lib [] tys

-- | Creates a /main/ function for the interpreter obtained by extending
-- the main interpreter with funcons, 'EntityDefaults' and a 'TypeRelation'. 
mkMainWithLibraryEntitiesTypes :: FunconLibrary -> EntityDefaults -> TypeRelation -> IO ()
mkMainWithLibraryEntitiesTypes lib defaults tyenv = do   
    args <- getArgs
    runWithExtensions lib defaults tyenv args Nothing

-- | Creates a /main/ function for the interpreter aware of only
-- the given 'FunconLibrary', 'EntityDefaults' and 'TypeRelation'. 
mkFullyFreshInterpreter :: FunconLibrary -> EntityDefaults -> TypeRelation -> IO ()
mkFullyFreshInterpreter lib defaults tyenv = do   
    args <- getArgs
    runWithExtensionsNoNothing lib defaults tyenv args Nothing 

-- | Creates a /main/ function for the interpreter aware of only
-- the given 'FunconLibrary', 'EntityDefaults' and 'TypeRelation',
-- and the built-in types and operations. 
mkFreshInterpreter :: FunconLibrary -> EntityDefaults -> TypeRelation -> IO ()
mkFreshInterpreter lib defaults tyenv = do   
    args <- getArgs
    runWithExtensionsNoCore lib defaults tyenv args Nothing 

-- | Same as 'run', except receiving additional interpreter extensions as arguments.
-- Useful when a translation to 'Funcons' has been implemented in Haskell as
-- well as 'Funcons', entities or datatypes specific to the object language.
-- Includes the 'Funcons.Core' funcons.
runWithExtensions :: 
    FunconLibrary -> EntityDefaults -> TypeRelation -> [String] -> Maybe Funcons -> IO ()
runWithExtensions lib defaults tyenv = 
  runWithExtensionsNoCore 
    (libUnions [Funcons.Core.Library.funcons, lib]) 
    (concat [defaults, Funcons.Core.Library.entities])
    (typeEnvUnions [tyenv, Funcons.Core.Library.types])

-- | Same as 'run', except receiving additional interpreter extensions as arguments.
-- Useful when a translation to 'Funcons' has been implemented in Haskell as
-- well as 'Funcons', entities or datatypes specific to the object language.
-- Does not include the 'Funcons.Core' funcons.
runWithExtensionsNoCore :: 
    FunconLibrary -> EntityDefaults -> TypeRelation -> [String] -> Maybe Funcons -> IO ()
runWithExtensionsNoCore lib defaults tyenv = runWithExtensionsNoNothing full_lib defaults tyenv
 where  full_lib = libUnions    [lib
                                ,Funcons.EDSL.library
                                ,Funcons.Core.Manual.library]

-- | Same as 'run', except receiving additional interpreter extensions as arguments.
-- Useful when a translation to 'Funcons' has been implemented in Haskell as
-- well as 'Funcons', entities or datatypes specific to the object language.
-- Does not include the 'Funcons.Core' funcons.
runWithExtensionsNoNothing :: 
    FunconLibrary -> EntityDefaults -> TypeRelation -> [String] -> Maybe Funcons -> IO ()
runWithExtensionsNoNothing lib defaults tyenv = emulate lib defaults tyenv 


-- | 
-- Creates a main function by passing in a list of command line arguments 
-- and an optional initial 'Funcons' to execute. The 'Funcons' argument is optional
-- as one of the command line arguments may refer to an .fct file or .config
-- file that specifies an initial 'Funcons' term.
-- Useful when a translation to 'Funcons' has been implemented in Haskell.
run :: [String] -> Maybe Funcons -> IO ()
run = runWithExtensions libEmpty [] emptyTypeRelation 

------------------------------------------------------------------------------
--- running programs 
emulate lib defaults tyenv args mf0 = do
    (opts, unknown_opts) <- run_options args
    forM_ unknown_opts $ \arg -> do
        putStrLn ("unknown option: " ++ arg)
    case interactive_mode opts of 
        True    -> emulate' (fread (string_inputs opts) :: Name -> IO Funcons) lib defaults tyenv opts mf0
        False   -> emulate' (fread (string_inputs opts) :: Name -> SimIO Funcons) lib defaults tyenv opts mf0

emulate' :: Interactive m => (Name -> m Funcons) ->
            FunconLibrary -> EntityDefaults -> TypeRelation -> RunOptions -> Maybe Funcons -> IO ()
emulate' reader lib defaults tyenv opts mf0 = do
    -- the initial funcon term must be either given from a .fct file (Maybe Funcons)
    -- or specified in a configuration file
    let f0 = maybe (funcon_term opts) id mf0
        msos_ctxt = MSOSReader (RewriteReader lib tyenv opts f0 f0) emptyINH emptyDCTRL reader
    -- run the Interactive monad, returning in the evaluation results + entity values.
    -- if in --interactive-mode the Interactive monad will be IO 
    --  and all the desired output will already have been printed to the screen
    ((e_exc_f, mut, wr), rem_ins) <- 
        fexec (runMSOS (setEntityDefaults defaults (stepTrans opts 0 (toStepRes f0)))
                msos_ctxt (emptyMSOSState {inp_es = inputs})) (inputValues opts)
    -- if not in --interactive-mode then print additional information based on flags
    unless (interactive_mode opts)
        (withResults defaults msos_ctxt e_exc_f mut wr rem_ins)
 where inputs = M.foldrWithKey op M.empty (inputValues opts)
                where   op nm _ = M.insert nm ([], Just (reader nm))

withResults defaults msos_ctxt e_exc_f msos_state wr rem_ins
        | show_tests opts =
            case e_exc_f of
             Left ie -> putStrLn (showIException ie)
             Right efvs -> printTestResults (either (:[]) (map FValue) efvs)
                              defaults msos_ctxt msos_state wr rem_ins
        | otherwise = do
    unless (show_output_only opts) $ do
        printCounts 
        case e_exc_f of
            Left ie -> putStrLn (showIException ie)
            Right f -> printResult (either (:[]) (map FValue) f)
        printMutable
        printControl
        printInput rem_ins (hide_input opts)
    printOutput
 where
    muts = mut_entities msos_state 
    opts = run_opts (ereader msos_ctxt)
    printResult f = when (show_result opts) $ do
                        putStrLn "Result:"
                        putStrLn (ppFunconsSeq opts f)
                        putStrLn ""

    printCounts = do
      when (show_counts opts) $ do 
        when (csv_output_with_keys opts) (putStrLn counterKeys) 
        if (csv_output opts) 
          then putStrLn $ displayCounters (counters (ewriter wr))
          else putStrLn $ ppCounters (counters (ewriter wr))

    printMutable = forM_ toShow display
     where toShow = show_mutable opts
           display name = case M.lookup name muts of
                            Nothing -> return ()
                            Just v ->  putStrLn ("Mutable Entity: " ++ unpack name) >>
                                       putStrLn (displayValue v) >> putStrLn ""

    printControl = forM_ (M.keys ctrl \\ toHide) display
     where ctrl = ctrl_entities wr
           toHide = hide_control opts
           display name = case M.lookup name ctrl of
                            Just (Just v) -> do
                                putStrLn ("Control Entity: " ++ unpack name) 
                                putStrLn (displayValue v) >> putStrLn ""
                            _             -> return ()

    printOutput = forM_ (M.keys out \\ toHide) display
     where out = out_entities wr
           display name = do
                    unless (show_output_only opts) 
                        (putStrLn ("Output Entity: " ++ unpack name))
                    case all isString_ vs && pp_string_outputs opts of
                        True    -> mapM_ (putStr . unString) vs
                        False   -> putStrLn (displayValues vs)
                    unless (show_output_only opts) (putStrLn "")
            where vs = out M.! name
           toHide = hide_output opts


    printInput ios toHide = forM_ (M.keys ios \\ toHide) display
     where display name = unless (null vs) $ do 
                            putStrLn ("Input Entity: " ++ unpack name)
                            putStrLn (displayValues vs)
                            putStrLn ""
            where vs = ios M.! name

    displayValues vs = intercalate "," (map displayValue vs)
    displayValue (Map m) = intercalate "\n" 
                                   [ displayValue key ++ " |-> " ++ displayValues val 
                                   | (key, val) <- M.assocs m ]
    displayValue (ADTVal "variable" [FValue (Atom a)
                                     ,FValue (ComputationType (Type t))]) = 
        "variable(" ++ displayValue (Atom a) ++ ", " ++ ppTypes (ppFuncons opts) t ++ ")"
    displayValue (Atom a) = a
    displayValue val | isString_ val = show (unString val)
    displayValue val = ppValues (ppFuncons opts) val

printTestResults :: [Funcons] -> EntityDefaults -> MSOSReader m -> 
                        MSOSState m -> MSOSWriter -> InputValues -> IO ()
printTestResults fs defaults msos_ctxt msos_state wr rem_ins = do
        forM_ (M.keys opts) printNotExists
        when (M.member "result-term" opts) $
            unless (result_term == fs) (reportError "result-term" (showFunconsSeq result_term) (showFunconsSeq fs))
        printMutable
        printControl
        printInputOutput out
        printInputOutput rem_ins

    where   eval_ctxt = ereader msos_ctxt
            muts = mut_entities msos_state
            eval_state = estate msos_state
            localEval name term = case runRewrite (rewriteFuncons term) eval_ctxt eval_state
                of  (Left ie,_,_) -> error ("internal exception in " ++ unpack name 
                                    ++ " evaluation:\n" ++ showIException ie)
                    (Right (ValTerm [v]),_,_) -> v
                    (Right (ValTerm vs),_,_) -> error 
                      ("evaluation of " ++ unpack name ++ " results in sequence")
                    (Right _,_,_) ->
                         error ("evaluation of " ++ unpack name ++ " requires step")
    
            mLocalEval term = case runRewrite(rewriteFuncons term) eval_ctxt eval_state of
                (Right (ValTerm [v]),_,_) -> Just v
                _                         -> Nothing

            result_term = case sequence (fmap recursiveFunconValue rf) of
                Nothing -> case sequence (fmap mLocalEval rf) of
                                Nothing -> rf
                                Just vs -> map FValue vs
                Just vs -> map FValue vs
             where rf = (opts M.! "result-term")
            opts = expected_outcomes (run_opts eval_ctxt)

            reportError name expected actual = do
                putStrLn ("expected " ++ unpack name ++ ": " ++ expected)
                putStrLn ("actual   " ++ unpack name ++ ": " ++ actual)

            printNotExists "result-term" = return ()
            printNotExists name = 
                case (M.lookup name muts, M.lookup name out
                     ,M.lookup name ctrl, M.lookup name rem_ins) of
                    (Nothing, Nothing, Nothing, Nothing) -> 
                        putStrLn ("unknown entity: " ++ unpack name)
                    _ -> return ()
        
            printMutable = forM_ (M.assocs muts) (uncurry display)
             where display name val = case M.lookup name opts of
                        Nothing -> return ()
                        Just expected -> unless 
                                            (map (localEval name) expected == [val]) 
                                            (reportError name (showL $ map showFuncons expected) (showValues val))

            -- set default values of output and control entities
            ctrl :: M.Map Name (Maybe Values)
            ctrl = foldr op (ctrl_entities wr) defaults
             where  op (DefControl name) ctrl 
                        | not (M.member name ctrl) = M.insert name Nothing ctrl
                    op _ ctrl = ctrl

            out = foldr op (out_entities wr) defaults
             where  op (DefOutput name) out 
                        | not (M.member name out) = M.insert name [] out
                    op _ out = out

            -- TODO this does not test the case that a control signal is expected 
            --      according to the test, but not present.
            printControl = forM_ (M.assocs ctrl) (uncurry display)
             where  display :: Name -> Maybe Values -> IO ()
                    -- test whether control signal is expected when there is none
                    -- shows expected signal 
                    display name Nothing = case M.lookup name opts of 
                        Nothing -> return ()
                        Just vals -> putStrLn ("expected "++unpack name++": "
                                        ++ showL (map (showValues . localEval name) vals))
                    -- test whether control signal is expected when there is one
                    -- shows that the emitted signal was unexpected
                    -- if a signal was expected, shows if actual and expected are unequal
                    display name (Just val) = case M.lookup name opts of
                        Nothing -> putStrLn ("unexpected " ++ unpack name ++ ": " ++ showValues val)
                        Just expected -> unless 
                                            (map (localEval name) expected == [val]) 
                                            (reportError name (showL $ map showFuncons expected) (showValues val))

            printInputOutput remaining = forM_ (M.assocs remaining) (uncurry display)
             where  -- no test-error if the input/output is empty 
                    -- (and no input/output was specified)
                    display name [] | Nothing <- M.lookup name opts = return ()
                    display name vals = case M.lookup name opts of
                        Nothing -> putStrLn ("unexpected " ++ unpack name ++ ": " ++ showL (map showValues vals))
                        Just expected -> case map (localEval name) expected of 
                            [ADTVal "list" exps] -> unless (exps == map FValue vals) (reportError name (showL $ map showFuncons exps) (showL $ map showValues vals))
                            val -> error ("non-list given as expected output entity ("++
                                        unpack name ++ "): " ++ showL (map showValues val))
                    
showL :: [String] -> String
showL elems = "[" ++ intercalate "," elems ++ "]"


-- $moduledoc
-- This module exports functions for creating executables for funcon interpeters.
-- The executables accepts a number of command line and configuration options that
-- are explained here. The /funcons-tools/ package exports an interpreter for
-- the core library of reusable funcons. This executable is called /runfct/ and is used
-- as an example here.
--
-- @ dist\/build\/runfct\/runfct \<options\>@
--
-- === Options
--  Options are used to change the behaviour of the interpreter and to change the
--  output the interpreter provides after execution.
--  An option is either a boolean-option, a string-option, a .config file or a .fct file.
--  All command line flags are considered from left to right,
--    each changing the current configuration.
--
-- (1) __Funcon term file__: A file containing a funcon term. (must have .fct extension). 
--        These files contain funcon terms written 
--          in prefix form with parentheses surrounding comma-separated arguments,
--          e.g. integer-multiply(6,7). The parser also accepts notation for lists, 
--          tuples, sets and map. For example, @[1,2,3]@, @(1,2,3)@, @&#x7b;1,2,3&#x7d;@,
--          and @&#x7b;1 |-> true, 2 |-> false, 3 |-> true &#x7d;@ respectively.
--  
-- (2) __Configurations file__: A file containing configuration options (see below).
--          (must have .config extension)
--  
-- (3) __String options__ (comma-separate strings for multiple inputs):
--
--      * --display-mutable-entity \<string\>: by default mutable entities are not displayed
--            this option is used to display one or more mutable entities.
--
--      * --hide-output-entity \<string\>:
--            by default output entities are displayed when output is available.
--            this option is used to hide one or more output entities.
--
--      * --hide-control-entity \<string\>:
--            by default control entities are displayed when a signal is raised.
--            this option is used to hide one or more control entities .
--
--      * --hide-input-entity \<string\>:
--            by default input entities are displayed when input has not been consumed.
--            this option is used to hide one or more input entities.
--      * --max-restarts \<natural\>:
--          perform a maximum of `n` transitive steps, useful for debugging.
--
-- (4) __Boolean options__ (/true/, /false/ or no argument (/true/ by default)):
--
--      * --refocus \<bool\>: use refocusing, only valid under certain conditions.
--    
--      * --full-environments \<bool\>: when printing funcons, display environments 
--              using map-notation, by default an environment is printed as "...".
--    
--      * --hide-result \<bool\>: do not show the resulting funcon term.
--    
--      * --display-steps \<bool\>: show meta-information about the interpretation, 
--              e.g. the number of steps, rewrites and restarts. 
--    
--      * --no-abrupt-termination \<bool\>: disable abrupt termination (affects uncaught control signals).
--    
--      * --interactive-mode \<bool\>: use real I/O for input and output.
--              By default I/O is simulated and all input is expected to be 
--              provided in a configuration file (see below) and output is collected
--              and displayed after execution is finished.
--              In interactive mode, the user has to provide input via the standard input,
--              and output is printed to the standard output as soon as it is available.
--    
--      * --string-inputs \<bool\>: by default input is parsed into a 'Values'.
--            This option tells the interpreter to yield the given string as input.
--    
--      * --format-string-outputs \<bool\>: if all output values are strings (and with this option on),
--            any escape characters are interpreted (e.g. "\\n" becomes an actual newline), and
--            the strings are concatenated and not enclosed in double quotes.
--    
--      * --hide-tests \<bool\>: do not execute tests (by default tests are executed if specified in a configuration file).
--    
--      * --show-output-only \<bool\>: print only output (omits all other information).
--    
--      * --auto-config \<bool\>: if a .fct file is given, search for a .config file
--            with the same name and apply it (on by default).
--     
-- === Configuration files
--  A configuration file is a sequence of 'fragments', where each fragment is of the form:
--
-- > <group> {
-- >    <keyvalue>*
-- > }
-- 
--  A \<keyvalue\> is a colon separated key-value pair, closed off by a semicolon, e.g.
-- 
-- > hide-control-entity: failed,thrown;
--
--  There are 4 valid groups: /general/, /funcons/, /tests/ and /inputs/.
--  
-- (1) __general__:
--    The general /group/ is used as an alternative to command line flags,
--    All Boolean and string options are available.
--    Additionally, the option "funcon-term" is available for giving an initial 
--    funcon term:
--
--        > general {
--        >     funcon-term: integer-add(3,2);
--        > }
--
-- (2) __funcons__:
--    This group is used to define simple (nullary) funcons.
--    They key is the name of the funcon,
--    the value is a funcon term to which the funcon will rewrite once evaluated.
--    Keys and values are separated by '=' in this group. This group is useful
--    to choose an interpretation for unspecified components of a language specification.
--    For example (from a Caml Light specification):
--
--         > funcons {
--         >     implemented-vectors        = vectors(values);
--         >     implemented-floats-format  = binary64;
--         >     implemented-integers-width = 31;
--         >     implemented-characters     = unicode-characters;
--         >     implemented-strings        = strings;
--         > }
--
-- (3) __tests__:
--    With this group unit-tests can be defined.
--    Each key-value pairs specifies the expected value of a semantic entities,
--    where the key is the name of a semantic entity
--    and the value is the expected value.
--    Additionally, the key "result-term" can be used to specify the expected result term.
--    The tests group is useful to specify a complete unit-test in a single file, e.g.
--
--         > general {
--         >     funcon-term: else(integer-add(integer-add(2,3),fail),print(3));
--         > }
--         > tests {
--         >    result-term: ();
--         >    standard-out: [3];
--         > }
--
-- (4) __inputs__:
--    The inputs group is used to specify default values for input entities, e.g.
--
--         > inputs {
--         >     standard-in: [1,2,3];
--         > }
--
--        When input entities are given default values, simulation mode is turned on
--        (even if --interactive-mode is used).
-- 
-- === Languages specific interpreters
--    This package does not provide just one interpreter, it provides
--    the ability to play `mix and match' with 'FunconLibrary's to form interpreters.
--    This enables the creation of interpreters for object languages from funcons
--    (entities, or datatypes) specific to that object language.
-- 
--    For this purpose, this module exports 'mkMainWithLibraryEntitiesTypes' (and variants). 
--    Say that a module exporting
--    a 'FunconLibrary' is a "funcon module".
--    An interpreter is obtained by importing the chosen "funcon modules" and uniting 
--    their 'FunconLibrary's (with 'libUnions'), perhaps together with default
--    values for entities ('EntityDefault') and information about custom datatypes ('TypeRelation').
--    The resulting maps are given as arguments to 'mkMainWithLibraryEntitiesTypes'
--    (or variant).
--    By using 'mkMainWithLibraryEntitiesTypes', all interpreters inherit the 
--        core reusable funcon library.



