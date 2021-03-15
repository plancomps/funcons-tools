
module Funcons.Exceptions where

import Funcons.Types
import Funcons.Printer

import Data.List (intercalate)
import Data.Text (unpack)

-- handling exception from the interpreter
type IException = (Funcons, Funcons, IE) --global, local, exception
data IE = SortErr String
        | Err String --TODO when used?
        | PartialOp String
        | Internal String 
        | NoRule [IException] 
        | NoMoreBranches [IException]
        | SideCondFail String
        | InsufficientInput Name
        | InsufficientInputConsumed Name
        | PatternMismatch String
        | StepOnValue [Values]

showIException :: IException -> String
showIException (f0,f,ie@(NoRule _)) = show ie
showIException (f0,f,ie@(NoMoreBranches _)) = show ie
showIException (f0,f,ie) = "Internal Exception: " ++ show ie ++ " on \n" ++ showFuncons f

instance Show IE where
    show (SortErr err) = "dynamic sort check (" ++ err ++ ")"
    show (NoRule []) = "no more rules to try"
    show (NoRule errs) = mkRulesErr (zipWith mkRuleErr [1..] errs) f 
      where (_,f,_) = head errs
    show (Err err)   = "exception (" ++ err ++ ")"
    show (Internal err)    = "exception (" ++ err ++ ")"
    show (SideCondFail str)         = str
    show (PatternMismatch str)  = str
    show (InsufficientInput nm) = "insufficient supply for " ++ unpack nm
    show (InsufficientInputConsumed nm) = "insufficient input consumed for entity " ++ unpack nm
    show (PartialOp str) = "partial operation"
    show (NoMoreBranches []) = "no more branches to try"
    show (NoMoreBranches errs) = mkRulesErr (zipWith mkRuleErr [1..] errs) f 
      where (_,f,_) = head errs
    show (StepOnValue v) = "attempting to step a value: " ++ showValuesSeq v

mkRuleErr i (_,_,ie) = show ie
mkRulesErr strs f = "  " ++ showFuncons f ++ ":\n" ++ intercalate "\n" (map ("    " ++) strs)

-- which exceptions stop a rule from executing so that the next one can be attempted?
failsRule :: IException -> Bool
failsRule (_,_,SideCondFail _)      = True
failsRule (_,_,PatternMismatch _)   = True
failsRule (_,_,SortErr  _)          = True
failsRule (_,_,PartialOp  _)        = True
failsRule (_,_,StepOnValue _)       = True
failsRule (_,_,NoMoreBranches _)    = True
failsRule (_,_,NoRule _)            = True 
failsRule _                         = False


