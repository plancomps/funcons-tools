{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.MultisetsBuiltin where

import Funcons.EDSL
import qualified Funcons.Operations as VAL 

library = libFromList [
      ("multisets", fromValOp multisets_ VAL.multisets_)
    , ("multiset", fromValOp multiset_ VAL.multiset_)
    , ("multiset-elements", fromValOp multiset_elements_ VAL.multiset_elements_)
    , ("multiset-occurrences", fromValOp multiset_occurrences_ VAL.multiset_occurrences_)
    , ("multiset-insert", fromValOp multiset_insert_ VAL.multiset_insert_)
    , ("multiset-delete", fromValOp multiset_delete_ VAL.multiset_delete_)
    , ("is-submultiset", fromValOp is_submultiset_ VAL.is_submultiset_)
    ]

multisets_ = applyFuncon "multisets"
multiset_ = applyFuncon "multiset"
multiset_elements_ = applyFuncon "multiset-elements"
multiset_occurrences_ = applyFuncon "multiset-occurrences"
multiset_insert_ = applyFuncon "multiset-insert"
multiset_delete_ = applyFuncon "multiset-delete"
is_submultiset_ = applyFuncon "is-submultiset"

