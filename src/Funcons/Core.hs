
-- | 
-- This module exports smart constructors for building funcon terms from a large
-- collections of funcons. 
-- Module "Funcons.EDSL" can be used to construct funcons.
-- Module "Funcons.Tools" provides functions for creating executables.
--
-- Apologies for the disorganisation of this file, most of its exports
--  exports have been generated.
--
--If a funcon is called 'handle-thrown', its smart constructor is called 
--'handle_thrown_' (hyphens replaced by underscores and an additional underscore
--at the end). Each smart constructors has a single argument, a list
--(of type (['Funcons']) representing the actual arguments of a funcon application.
-- For example, the funcon 'integer-add' can be applied to an arbitrary number
--of (integer) arguments, e.g. 'integer_add_' ['int_' 3, 'int_' 4, 'int_' 5].
module Funcons.Core (
    int_, nat_,
    module Funcons.Core.Library,
    module Funcons.Core.Manual) where

import Funcons.Types hiding (set_) -- Haddock dependency
import Funcons.Core.Library 
import Funcons.Core.Manual hiding (unicode_, ascii_character_)


