{-# LANGUAGE TupleSections, FlexibleInstances, OverloadedStrings,TypeSynonymInstances #-}

module Funcons.Simulation where

import Funcons.Types
import Funcons.Exceptions
import Funcons.Printer
import Funcons.Parser (fvalue_parse)
import Funcons.RunOptions

import Control.Applicative
import Control.Monad.State
import System.IO (hFlush,stdout)
import qualified Data.Map as M
import Data.Text (unpack)

class Monad m => Interactive m where
    fread    :: Bool -> Name {- entity name -} -> m Funcons
    fprint   :: Name -> Values -> m ()
    fexec    :: m a -> InputValues -> IO (a, InputValues)

instance Interactive IO where
    fexec ma _ = (,M.empty) <$> ma

    fread str_inp nm = (case nm of
        "standard-in" -> putStr "\n> " >> hFlush stdout
        _ -> putStrLn ("Please provide input for " ++ unpack nm ++ ":"))
                >> getLine >>= return . toFuncon
        where   toFuncon  str | str_inp   = string_ str
                              | otherwise = fvalue_parse str

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

