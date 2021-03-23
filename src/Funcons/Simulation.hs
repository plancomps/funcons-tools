{-# LANGUAGE TupleSections, FlexibleInstances, OverloadedStrings,TypeSynonymInstances #-}

module Funcons.Simulation where

import Funcons.Types
import Funcons.Exceptions
import Funcons.Printer
import Funcons.Parser (fvalue_parse_either)
import Funcons.RunOptions

import Control.Applicative
import Control.Monad.State
import System.Console.Haskeline
import qualified Data.Map as M
import Data.Text (unpack)

class Monad m => Interactive m where
    fread    :: Bool -> Name {- entity name -} -> m Funcons
    fprint   :: Name -> Values -> m ()
    fexec    :: m a -> InputValues -> IO (a, InputValues)

instance Interactive IO where
    fexec ma _ = (,M.empty) <$> ma

    fread str_inp nm = runInputT defaultSettings $ do
        mLine <- getInputLine prompt 
        case mLine of Nothing -> return (string_ "")
                      Just s  -> toFuncon s
        where   toFuncon  str | str_inp   = return (string_ str)
                              | otherwise = case fvalue_parse_either str of
                                  Left err -> lift (putStrLn err >> fread str_inp nm)
                                  Right f  -> return f
                prompt | nm == "standard-in" = "Please provide a literal value\n> "
                       | otherwise =  "Please provide a literal value for " ++ unpack nm ++ ":"

    fprint _ v | isString_ v  = putStr (unString v)
               | otherwise    = putStr (showValues v)

type SimIO = State InputValues 
runSimIO :: SimIO a -> InputValues -> (a, InputValues)
runSimIO = runState 

instance Interactive SimIO where
    fexec ma defs = return (runState ma defs)
    
    fread _ nm = do v <- gets mLookup 
                    modify (M.adjust mTail nm)
                    return v
        where mLookup m = case M.lookup nm m of
                  Just (v:_)  -> FValue v
                  _           -> FValue null_value__ 
              mTail []     = []
              mTail (_:vs) = vs

    -- SimIO ignores prints as simulated Output is always done
    -- alternative is to use tell from Writer monad here and somehow remember
    -- whatever has been printed in Interactive instance for IO
    -- (necessary for observing printed output by funcon defs, using 'readOUT')
    fprint nm v = return ()
-----------

