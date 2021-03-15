{-# LANGUAGE OverloadedStrings, TupleSections, FlexibleInstances #-}

module Funcons.Types (
  module Funcons.Types,
  module VAL,) where

import qualified Funcons.Operations as VAL hiding (SortErr, ValueOp)
import Funcons.Operations hiding (Name, Values, ComputationTypes, Types, isMap, isNull, isSet, map_empty_, isEnv, isDefinedVal, isChar, isVec, isType, isList, isNat, isInt, atoms_, integers_, values_, set_, list_, tuple_, atom_, nulltype_, non_null_values_, types_, value_types_, toList, isList, libFromList, listUnites, null)

import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.MultiSet as MS
import qualified Data.Vector as V
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Ratio

type MetaVar = String
type Name = Text

-- |
-- Internal representation of funcon terms.
-- The generic constructors 'FName' and 'FApp' use names to represent
-- nullary funcons and applications of funcons to other terms.
-- Funcon terms are easily created using 'applyFuncon' or via
-- the smart constructors exported by "Funcons.Core".
data Funcons    = FName Name
                | FApp Name [Funcons]
--                | FTuple [Funcons]
--                | FList [Funcons]
                | FSet [Funcons]
                | FMap [Funcons]
                | FBinding Funcons [Funcons] -- required for map-notation
                | FValue Values
                | FSortSeq Funcons VAL.SeqSortOp
                | FSortPower Funcons Funcons {- evals to natural number -}
                | FSortUnion Funcons Funcons
                | FSortInter Funcons Funcons
                | FSortComplement Funcons
                | FSortComputes Funcons
                | FSortComputesFrom Funcons Funcons 
                deriving (Eq, Ord, Show, Read)

-- |
-- Build funcon terms by applying a funcon name to `zero or more' funcon terms.
-- This function is useful for defining smart constructors, e,g,
--
-- > handle_thrown_ :: [Funcons] -> Funcons
-- > handle_thrown_ = applyFuncon "handle-thrown"
--
-- or alternatively,
--
-- > handle_thrown_ :: Funcons -> Funcons -> Funcons
-- > handle_thrown_ x y = applyFuncon "handle-thrown" [x,y]
applyFuncon :: Name -> [Funcons] -> Funcons
applyFuncon str args | null args = FName str
                     | otherwise = FApp str args

tuple_ :: [Funcons] -> Funcons
tuple_ = applyFuncon "tuple" 

-- | Creates a list of funcon terms.
list_ :: [Funcons] -> Funcons
list_ = applyFuncon "list" 

-- | Creates a set of funcon terms.
set_ :: [Funcons] -> Funcons
set_ = applyFuncon "set"

-- | Funcon term representation identical to 'Funcons',
-- but with meta-variables.
data FTerm  = TVar MetaVar
            | TName Name
            | TApp Name [FTerm]
            | TSeq [FTerm] -- a sequence of terms, consumed during substitution
--            | TList [FTerm]
            | TSet  [FTerm]
            | TMap  [FTerm]
            | TBinding FTerm FTerm -- latter can be a sequence
            | TFuncon Funcons
            | TSortSeq FTerm VAL.SeqSortOp
            | TSortPower FTerm FTerm {- should eval to a natural number -}
            | TSortUnion FTerm FTerm
            | TSortInter FTerm FTerm  
            | TSortComplement FTerm
            | TSortComputes FTerm
            | TSortComputesFrom FTerm FTerm
            | TAny -- used whenever funcon terms may have holes in them
                   -- currently only the case in "downwards" flowing signals
            deriving (Eq, Ord, Show, Read)


type Values = VAL.Values Funcons

instance HasValues Funcons where
  inject = FValue
  project f = case f of
    FValue v  -> Just v
    _         -> Nothing

type Map        = M.Map Values Values
type Set        = S.Set Values
type Vectors    = V.Vector Values

-- | Representation of builtin types.
type ComputationTypes = VAL.ComputationTypes Funcons
type Types   = VAL.Types Funcons
type TTParam = (Types,Maybe VAL.SeqSortOp)

binary32 :: Values
binary32 = ADTVal "binary32" []

binary64 :: Values
binary64 = ADTVal "binary64" []

adtval :: Name -> [Values] -> Values
adtval nm = ADTVal nm . map FValue

tuple_val__ :: [Values] -> Values
tuple_val__ = adtval "tuple"
tuple_val_ = FValue . tuple_val__

nullaryTypes :: [(Name,Types)]
nullaryTypes =
  [ ("algebraic-datatypes", ADTs)
  , ("adts"               , ADTs)
--  , ("naturals",            Naturals)
--  , ("nats",                Naturals)
  , ("rationals",           Rationals)
  , ("values",              VAL.Values)
  ]

unaryTypes :: [(Name,Types->Types)]
unaryTypes =
  [ ("multisets", multisets)
--  , ("lists",     Lists)
--  , ("vectors",   Vectors)
  ]

binaryTypes :: [(Name,Types->Types->Types)]
binaryTypes =
  []

boundedIntegerTypes :: [(Name, Integer -> Types)]
boundedIntegerTypes = []

floatTypes :: [(Name, IEEEFormats -> Types)]
floatTypes = [("ieee-floats", IEEEFloats)]

--- smart constructors for values

-- | Creates an integer 'literal'.
int_ :: Int -> Funcons
int_ = FValue . mk_integers . toInteger

-- | Creates a natural 'literal'.
nat_ :: Int -> Funcons
nat_ i | i < 0      = int_ i
       | otherwise  = FValue $ mk_naturals $ toInteger i

bool_ :: Bool -> Funcons
bool_ = FValue . bool__

bool__ :: Bool -> Values 
bool__ = VAL.tobool

-- | Creates an atom from a 'String'.
atom_ :: String -> Funcons
atom_ = FValue . Atom

-- | Creates a string literal.
string_ :: String -> Funcons
string_ = FValue . string__
string__ :: String -> Values
string__ = ADTVal "list" . map char_ 

char_ :: Char -> Funcons
char_ = FValue . char__

char__ :: Char -> Values
char__ = mk_unicode_characters 

list__ :: [Values] -> Values
list__ = VAL.list

vector__ :: [Values] -> Values
vector__ = VAL.vector

tuple__ :: [Values] -> Values
tuple__ = VAL.tuple


float_ :: Double -> Funcons
float_ = FValue . Float

ieee_float_32_ :: Float -> Funcons
ieee_float_32_ = FValue . IEEE_Float_32
ieee_float_64_ :: Double -> Funcons
ieee_float_64_ = FValue . IEEE_Float_64

-- | The empty map as a 'Funcons'.
empty_map_,map_empty_ :: Funcons
empty_map_ = FValue (Map M.empty)
map_empty_ = empty_map_

-- | The empty set as a 'Funcons'.
empty_set_ :: Funcons
empty_set_ = FValue (Set S.empty)

-- | Creates a tuple of funcon terms.
--tuple_ :: [Funcons] -> Funcons
--tuple_ = FTuple

type_ :: Types -> Funcons
type_ = FValue . typeVal

sort_ :: ComputationTypes -> Funcons
sort_ = FValue . ComputationType

comp_type_ :: ComputationTypes -> Funcons
comp_type_ = FValue . ComputationType 

vec :: V.Vector (Values) -> Funcons
vec = FValue . Vector

vec_ :: [Values] -> Funcons
vec_ = FValue . Vector . V.fromList
-- idval :: Values -> Values
-- idval = ID . ID'

typeVal :: Types -> Values
typeVal = ComputationType . Type

fvalues :: [Values] -> [Funcons]
fvalues = map FValue

listval :: [Values] -> Funcons
listval = FValue . ADTVal "list" . map FValue

setval :: [Values] -> Funcons
setval = FValue . setval_

setval_ :: [Values] -> Values
setval_ = Set . S.fromList

mapval :: [Values] -> Funcons
mapval = FValue . mapval_

mapval_ :: [Values] -> Values
mapval_ mvs = case mapM fromBinding mvs of 
  Just vs -> Map $ M.fromListWith const vs
  _       -> error "mapval: invalid map-notation"

fromBinding :: Values -> Maybe (Values, [Values])
fromBinding (ADTVal "tuple" (k':vs')) = do
  k   <- project k'
  vs  <- mapM project vs' 
  return (k,vs)
fromBinding k = Just (k,[])
 
-- subtyping rationals

{-
mk_rationals :: Rational -> Values
mk_rationals r  | denominator r == 1 = mk_integers (numerator r)
                | otherwise             = Rational r

mk_integers :: Integer -> Values
mk_integers i   | i >= 0    = mk_naturals i
                | otherwise = Int i

mk_naturals :: Integer -> Values
mk_naturals = Nat

-}

-- | Returns the /unicode/ representation of an assci value.
-- Otherwise it returns the original value.
--- Value specific

-- | Attempt to downcast a funcon term to a value.
downcastType :: Funcons -> Types 
downcastType (FValue v) = downcastValueType v
downcastType _ = error "downcasting to sort failed"

downcastSort :: Funcons -> ComputationTypes 
downcastSort (FValue (ComputationType s)) = s
downcastSort _ = error "downcasting to sort failed"

downcastValue :: Funcons -> Values
downcastValue (FValue v) = v
downcastValue _ = error "downcasting to value failed"

recursiveFunconValue :: Funcons -> Maybe Values
recursiveFunconValue (FValue v) = Just v
--recursiveFunconValue (FList fs) = List <$> mapM recursiveFunconValue fs
recursiveFunconValue (FSet fs)  = Set . S.fromList <$> mapM recursiveFunconValue fs
recursiveFunconValue (FMap fs)  = do
  vs      <- mapM recursiveFunconValue fs
  assocs  <- mapM fromBinding vs
  return (Map $ M.fromList assocs)
recursiveFunconValue _ = Nothing

allEqual :: [Values] -> [Values] -> Bool
allEqual xs ys = length xs == length ys && and (zipWith (===) xs ys)

allUnEqual :: [Values] -> [Values] -> Bool
allUnEqual xs ys = length xs /= length ys || or (zipWith (=/=) xs ys)

isNull :: Funcons -> Bool
isNull (FValue n) = n == null__
isNull _ = False

hasStep (FValue _) = False 
hasStep _ = True
isVal (FValue _) = True
isVal _ = False
isDefinedVal f = isVal f && not (isNull f)

-- functions that check simple properties of funcons
-- TODO: Some of these are used, and all are exported by Funcons.EDSL
--       But are all of them still needed.  E.g isId doesn't seem very useful now that ids are just strings.
isString (FValue v)             = isString_ v
isString _                      = False
isChar (FValue v)               = isJust (upcastCharacter v)
isChar _                        = False
isNat (FValue (Int _))          = True
isNat _                         = False
isInt (FValue (Int _))           = True
isInt _                         = False
isList (FValue (ADTVal "list" _)) = True
isList _                        = False
isEnv f                         = isMap f
isMap (FValue (Map _))           = True
isMap _                          = False
isSet (FValue (Set _))           = True
isSet _                         = False
isTup _                         = False
isSort (FValue (ComputationType _)) = True
isSort _                        = False
isSort_ (ComputationType _) = True
isSort_ _                        = False
isType (FValue (ComputationType (Type _))) = True
isType _                        = False
isVec (FValue (Vector _))        = True
isVec _                         = False
isCharacter_ v           = isJust (upcastCharacter v)

integers_,values_ :: Funcons
integers_ = type_ Integers
values_ = type_ VAL.Values
nulltype_ = type_ NullType
vectors_ :: Types -> Funcons
vectors_ = type_ . vectors  

-- type environment

-- | The typing environment maps datatype names to their definitions.
type TypeRelation = M.Map Name DataTypeMembers

-- | A type parameter is of the form X:T where the name of the parameter,/X/, is optional.
-- When present, /X/ can be used to specify the type of constructors.
-- Variable /X/ be a sequence variable.
type TypeParam  = (Maybe MetaVar, Maybe VAL.SeqSortOp, FTerm)
data TPattern   = TPWildCard
                | TPVar MetaVar 
                | TPSeqVar MetaVar VAL.SeqSortOp
                | TPLit FTerm {- should rewrite to a type -}
                | TPComputes TPattern
                | TPComputesFrom TPattern TPattern
                | TPADT Name [TPattern]
                deriving (Show, Eq, Ord, Read)

-- | A datatype has `zero or more' type parameters and
-- `zero or more' alternatives.
data DataTypeMembers = DataTypeMemberss Name [TPattern] [DataTypeAltt]
                     deriving (Show)

-- | An alternative is either a datatype constructor or the inclusion
-- of some other type. The types are arbitrary funcon terms (with possible
-- variables) that may require evaluation to be resolved to a 'Types'.
data DataTypeAltt = DataTypeInclusionn FTerm
                  | DataTypeMemberConstructor Name [FTerm] (Maybe [TPattern])
                  deriving (Show)

-- | Lookup the definition of a datatype in the typing environment.
typeLookup :: Name -> TypeRelation -> Maybe DataTypeMembers
typeLookup = M.lookup

-- | The empty 'TypeRelation'.
emptyTypeRelation :: TypeRelation
emptyTypeRelation = M.empty

-- | Unites a list of 'TypeRelation's.
typeEnvUnions :: [TypeRelation] -> TypeRelation
typeEnvUnions = foldr typeEnvUnion emptyTypeRelation

-- | Unites two 'TypeRelation's.
typeEnvUnion :: TypeRelation -> TypeRelation -> TypeRelation
typeEnvUnion = M.unionWith (\_ _ -> error "duplicate type-name")

-- | Creates a `TypeRelation' from a list.
typeEnvFromList :: [(Name, DataTypeMembers)] -> TypeRelation
typeEnvFromList = M.fromList



