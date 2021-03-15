{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Composite.MapsBuiltin where

import Control.Applicative ((<$>))

import Funcons.EDSL hiding (map_)
import Funcons.MSOS (rewrittens)
import Funcons.Core.Values.Primitive.BoolBuiltin
import Funcons.Core.Values.Composite.SetsBuiltin

import qualified Funcons.Operations as VAL

import qualified Data.Set as S
import qualified Data.Map as M

import Data.Foldable (foldrM)

library = libFromList [
    ("map-empty", fromNullaryValOp map_empty_ VAL.map_empty_)
  , ("is-map-empty", fromValOp is_map_empty_ VAL.is_map_empty_)
  , ("map-insert", fromValOp map_insert_ VAL.map_insert_)
  , ("map-lookup", fromSeqValOp map_lookup_ VAL.map_lookup_)
  , ("lookup", fromSeqValOp map_lookup_ VAL.map_lookup_)
  , ("map-domain", fromValOp map_domain_ VAL.domain_)
  , ("domain", fromValOp map_domain_ VAL.domain_)
  , ("dom", fromValOp map_domain_ VAL.domain_)
  , ("map-delete", fromValOp map_delete_ VAL.map_delete_)
  , ("is-in-domain", fromValOp is_in_domain_ VAL.is_in_domain_)
  , ("map-unite", fromValOp map_unite_ VAL.map_unite_)
  , ("map-override", fromValOp map_override_ VAL.map_override_)
  , ("maps", fromValOp maps_ VAL.maps_)
  , ("map", fromValOp map_ VAL.map_)
  , ("map-elements", fromSeqValOp map_elements_ VAL.map_elements_)
  ]  

map_ = applyFuncon "map"
maps_ = applyFuncon "maps"
map_empty_ = applyFuncon "map-empty"
is_map_empty_ = applyFuncon "is-map-empty"
is_in_domain_ = applyFuncon "is-in-domain"
map_insert_ = applyFuncon "map-insert"
map_delete_ = applyFuncon "map-delete"
-- |
-- Computes the union over a sequence of maps.
-- If the maps do not have disjoint domains a failure signal is raised.
map_unite_ = FApp "map-unite"
lookup_ = applyFuncon "lookup"
map_lookup_ = applyFuncon "lookup"
map_override_ = applyFuncon "map-override"
domain_ = applyFuncon "domain"
dom_ = applyFuncon "domain"
map_domain_ = applyFuncon "domain"
map_elements_ = applyFuncon "map-elements"
