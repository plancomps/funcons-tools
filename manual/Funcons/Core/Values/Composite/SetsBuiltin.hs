{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.SetsBuiltin where

import Funcons.EDSL hiding (set_)
import qualified Funcons.Operations as VAL 

import qualified Data.Set as S

import Test.RandomStrings (randomString', randomASCII)
import System.IO.Unsafe (unsafePerformIO)

library = libFromList [
    ("set", fromValOp set_ VAL.set_)
  , ("set-empty", fromNullaryValOp set_empty_ VAL.set_empty_)
  , ("sets", fromValOp sets_ VAL.sets_)
  , ("is-in-set", fromValOp is_in_set_ VAL.is_in_set_)
  , ("set-elements", fromSeqValOp set_elements_ VAL.set_elements_)
  , ("is-subset", fromValOp is_subset_ VAL.is_subset_)  
  , ("set-insert", fromValOp set_insert_ VAL.set_insert_)
  , ("set-unite", fromValOp set_unite_ VAL.set_unite_)
  , ("set-intersect", fromValOp set_intersect_ VAL.set_intersect_)
  , ("set-difference", fromValOp set_difference_ VAL.set_difference_)
  , ("set-size", fromValOp set_size_ VAL.set_size_)
  , ("some-element", fromValOp some_element_ VAL.some_element_)
  , ("element-not-in", fromValOp element_not_in_ VAL.element_not_in_)
--  , ("is-set-empty", fromValOp is_set_empty_ VAL.is_set_empty_)
{-    ,   ("is-set-empty", ValueOp is_set_empty_op)
    ,   ("set-to-list", ValueOp stepSetToList)
    ,   ("list-to-set", ValueOp stepList_To_Set)-}
  ]

sets_ = applyFuncon "sets"
set_elements_ = applyFuncon "set-elements"
set_size_= applyFuncon "set-size"
set_intersect_= applyFuncon "set-intersect"
set_difference_ = applyFuncon "set-difference"
some_element_ = applyFuncon "some-element"
is_subset_ = applyFuncon "is-subset"
set_ = applyFuncon "set"
is_in_set_ = applyFuncon "is-in-set"
set_unite_ = applyFuncon "set-unite"
set_insert_ = applyFuncon "set-insert"
element_not_in_ = applyFuncon "element-not-in"
set_empty_ = applyFuncon "set-empty"
--is_set_empty_ = applyFuncon "is-set-empty"
--set_to_list_ = applyFuncon "set-to-list"

