{-# LANGUAGE OverloadedStrings, TupleSections, FlexibleContexts #-}

module Funcons.RunOptions where

import Funcons.Types
import Funcons.GLLParser (Parser(..), pFunconsSeq, pFuncons, fct_lexerSettings)
import Funcons.Parser (fct_parse)

import GLL.Combinators hiding (chooses)

import qualified Data.Map as M
import Control.Monad (when)
import Control.Compose (OO(..))
import Data.Text (pack)
import Data.List (isSuffixOf, isPrefixOf)
import Data.List.Split (splitOn)

import System.Directory (doesFileExist)

type GeneralOptions = M.Map Name String
type BuiltinFunconsOptions = M.Map Name Funcons
type TestOptions = M.Map Name [Funcons]
type InputValues = M.Map Name [Values]

data RunOptions = RunOptions {
            mfuncon_term        :: Maybe Funcons
        ,   general_opts        :: GeneralOptions
        ,   builtin_funcons     :: BuiltinFunconsOptions
        ,   expected_outcomes   :: TestOptions
        ,   given_inputs        :: InputValues
        }

defaultRunOptions :: RunOptions
defaultRunOptions = RunOptions Nothing M.empty M.empty M.empty M.empty

optionsOverride opts opts' = RunOptions
    (maybe (mfuncon_term opts) Just (mfuncon_term opts'))
    (general_opts opts `M.union` general_opts opts')
    (builtin_funcons opts `M.union` builtin_funcons opts')
    (expected_outcomes opts `M.union` expected_outcomes opts')
    (given_inputs opts `M.union` given_inputs opts')

funcon_term :: RunOptions -> Funcons
funcon_term = maybe err id . mfuncon_term
    where err = error "Please give a .fct file as an argument or use the --funcon-term flag"

bool_opt_default :: Bool -> Name -> M.Map Name String -> Bool
bool_opt_default def nm m = case M.lookup nm m of
    Nothing         -> def
    Just "false"    -> False
    _               -> True


bool_opt :: Name -> M.Map Name String -> Bool
bool_opt nm m = bool_opt_default False nm m

do_refocus :: RunOptions -> Bool
do_refocus opts = bool_opt_default True "refocus" (general_opts opts)

max_restarts :: RunOptions -> Maybe Int
max_restarts = fmap read . M.lookup "max-restarts" . general_opts

do_abrupt_terminate :: RunOptions -> Bool
do_abrupt_terminate = not . bool_opt "no-abrupt-termination" . general_opts

pp_full_environments :: RunOptions -> Bool
pp_full_environments = bool_opt "full-environments" . general_opts

show_result :: RunOptions -> Bool
show_result opts = if bool_opt "hide-result" (general_opts opts)
    then False
    else not (interactive_mode opts)

show_counts :: RunOptions -> Bool
show_counts opts = if bool_opt "display-steps" (general_opts opts)
    then not (interactive_mode opts)
    else False

show_mutable :: RunOptions -> [Name]
show_mutable = maybe [] (map pack . splitOn ",") . M.lookup "display-mutable-entity"  . general_opts

hide_output :: RunOptions -> [Name]
hide_output = maybe [] (map pack . splitOn ",") . M.lookup "hide-output-entity"  . general_opts

hide_input :: RunOptions -> [Name]
hide_input = maybe [] (map pack . splitOn ",") . M.lookup "hide-input-entity"  . general_opts

hide_control :: RunOptions -> [Name]
hide_control = maybe [] (map pack . splitOn ",") . M.lookup "hide-control-entity" . general_opts

interactive_mode :: RunOptions -> Bool
interactive_mode opts = 
  M.null (inputValues opts) && bool_opt "interactive-mode" (general_opts opts)

pp_string_outputs :: RunOptions -> Bool
pp_string_outputs = bool_opt "format-string-outputs" . general_opts

string_inputs :: RunOptions -> Bool
string_inputs = bool_opt "string-inputs" . general_opts

show_tests :: RunOptions -> Bool
show_tests opts = if bool_opt "hide-tests" (general_opts opts)
        then False
        else M.size (expected_outcomes opts) > 0

show_output_only :: RunOptions -> Bool
show_output_only opts = if bool_opt "show-output-only" (general_opts opts)
        then True
        else interactive_mode opts

auto_config :: RunOptions -> Bool
auto_config opts = bool_opt_default True "auto-config" (general_opts opts)

csv_output :: RunOptions -> Bool
csv_output opts = if bool_opt "csv" (general_opts opts)
                    then True
                    else csv_output_with_keys opts

csv_output_with_keys :: RunOptions -> Bool
csv_output_with_keys opts = bool_opt "csv-keys" (general_opts opts)

inputValues :: RunOptions -> InputValues
inputValues = given_inputs

booleanOptions = 
  ["refocus", "full-environments", "hide-result", "display-steps"
  ,"no-abrupt-termination", "interactive-mode", "string-inputs"
  ,"format-string-outputs", "hide-tests", "show-output-only"
  ,"auto-config", "csv", "csv-keys"]
booleanOptions_ = map ("--" ++) booleanOptions

stringOptions = ["display-mutable-entity", "hide-output-entity"
    , "hide-control-entity", "hide-input-entity", "max-restarts"]
stringOptions_ = map ("--" ++) stringOptions

allOptions = "funcon-term" : booleanOptions ++ stringOptions
allOptions_ = "--funcon-term" : booleanOptions_ ++ stringOptions_

run_options :: [String] -> IO (RunOptions, [String])
run_options = fold (defaultRunOptions, [])
 where  fold (opts,errors) (arg:args)
            | arg `elem` booleanOptions_ =
                let (val, rest)
                        | not (null args)
                        , not (isPrefixOf "--" (head args)) = (head args, tail args)
                        | otherwise = ("true", args)
                    opts' = opts {general_opts = M.insert (pack (tail (tail arg)))
                                    val (general_opts opts)}
                in fold (opts',errors) rest
            | arg `elem` stringOptions_ && length args > 0 =
                let opts' = opts {general_opts = M.insert (pack (tail (tail arg)))
                                    (head args) (general_opts opts)}
                in fold (opts', errors) (tail args)
            | arg == "--funcon-term" &&  length args > 0 =
                let opts' = opts {mfuncon_term = Just (fct_parse (head args))}
                in fold (opts', errors) (tail args)
            | isSuffixOf ".fct" arg = do
                fct <- readFile arg
                let cfg_name = take (length arg - 4) arg ++ ".config"
                exists <- doesFileExist cfg_name
                opts' <- if exists && auto_config opts
                            then readFile cfg_name >>=
                                    return . flip (parseAndApplyConfig cfg_name) opts
                            else return opts
                let opts'' = opts' {mfuncon_term = Just (fct_parse fct)}
                fold (opts'', errors) args
            | isSuffixOf ".config" arg = fold (opts, errors) ("--config":arg:args)
            | arg == "--config" && length args > 0 = do
                let cfg_name = head args
                exists <- doesFileExist cfg_name
                when (not exists) (error ("config file not found: " ++ cfg_name))
                str <- readFile cfg_name
                let opts' = parseAndApplyConfig cfg_name str opts
                fold (opts', errors) (tail args)
            | otherwise = do
                exists <- doesFileExist (arg++".fct")
                if exists then fold (opts, errors) ((arg++".fct"):args)
                          else fold (opts, arg:errors) args
        fold (opts, errors) [] = return (opts, errors)

parseAndApplyConfig :: FilePath -> String -> RunOptions -> RunOptions
parseAndApplyConfig fp str = optionsOverride (config_parser str)

-- gll config parser
config_parser :: String -> RunOptions
config_parser string = case GLL.Combinators.parseWithOptions [maximumPivot,throwErrors] pRunOptions
                             (Funcons.RunOptions.lexer string) of
  []      -> error "no parse (config)"
  (c:_)   -> c

lexer :: String -> [Token]
lexer = GLL.Combinators.lexer cfg_lexerSettings

cfg_lexerSettings = fct_lexerSettings {
      keywords = (keywords fct_lexerSettings) ++ cfg_keywords
  ,   keychars = (keychars fct_lexerSettings) ++ cfg_keychars
  }

cfg_keychars = ":;="
cfg_keywords = allOptions ++ ["result-term", "general", "tests", "funcons", "inputs"]

pRunOptions :: Parser RunOptions
pRunOptions = "SPECS"
  <:=> foldr optionsOverride defaultRunOptions <$$> multiple pSpec

pSpec :: Parser RunOptions
pSpec = "SPEC"
  <:=> keyword "general" **> braces pGeneral
  <||> keyword "tests" **> braces pTestOutcomes
  <||> keyword "funcons" **> braces pBuiltinFuncons
  <||> keyword "inputs" **> braces pInputValues

pGeneral :: Parser RunOptions
pGeneral = "GENERAL"
  <:=> toOpts <$$> --TODO uncomfortable usage of id
        optional (id <$$ keyword "funcon-term" <** keychar ':' <**> pFuncons <** keychar ';')
          <**> (M.fromList <$$> multiple pKeyValues)
 where toOpts mf gen = defaultRunOptions {mfuncon_term = mf, general_opts = gen}
       pKeyValues = "GENERAL-KEYVALUES" <:=> pBoolOpts <||> pStringOpts
        where   pBoolOpts = "GENERAL-BOOLS" `chooses` (map pKeyValue booleanOptions)
                 where pKeyValue key = (pack key,) . maybe "true" id
                            <$$ keyword key <**> optional (keychar ':' **> pBool)
                                  <** keychar ';'

                pStringOpts = "GENERAL-STRINGS" `chooses` (map pKeyValue stringOptions)
                 where pKeyValue key = (pack key,) <$$ keyword key <** keychar ':'
                                          <**> pStringValue <** keychar ';'

chooses p alts = (<::=>) p (OO alts)

pBool :: Parser String
pBool = "BOOL-VALUE" <:=> id_lit -- everything except `false` is considered `true`

pStringValue :: Parser String
pStringValue = "STRING-VALUE" <:=> id_lit <||> string_lit

pFunconName :: Parser String
pFunconName = "FUNCON-NAME" <:=> id_lit

pTestOutcomes :: Parser RunOptions
pTestOutcomes = "TEST-OUTCOMES"
  <:=> toOptions <$$> (M.union <$$> pResult <**> pEntityValues)
    where pResult = mStoreResult <$$>
                      optional (id <$$ keyword "result-term" <** keychar ':'
                                             <**> pFunconsSeq <** keychar ';')
            where mStoreResult Nothing = M.empty
                  mStoreResult (Just f) = M.singleton "result-term" f
          pEntityValues = "TEST-ENTITIES" <:=> M.fromList <$$> multiple
              ((,) . pack <$$> pFunconName <** keychar ':' <**> pFunconsSeq <** keychar ';')
          toOptions map = defaultRunOptions { expected_outcomes = map }

pBuiltinFuncons :: Parser RunOptions
pBuiltinFuncons = "BUILTIN-FUNCONS"
  <:=> insertFuncons <$$> multiple ((,) . pack <$$> pFunconName <** keychar '='
                            <**> pFuncons <** keychar ';')
 where insertFuncons list = defaultRunOptions {builtin_funcons = M.fromList list}

pInputValues :: Parser RunOptions
pInputValues = "INPUT-VALUES"
  <:=> insertInputs <$$> multiple (toPair <$$> pFunconName <** keychar ':'
                                  <**> pFunconsSeq <** keychar ';')
  where insertInputs list = defaultRunOptions { given_inputs = M.fromList list }
        toPair nm fs = case sequence (map recursiveFunconValue fs) of
                        Just vs -> (pack nm, vs)
                        _       -> error ("inputs for " ++ nm ++ " not a sequence of values")
