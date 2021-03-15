-- GeNeRaTeD fOr: ../../CBS-beta/Funcons-beta/Computations/Abnormal/Sticking.cbs
{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Computations.Abnormal.Sticking where

import Funcons.EDSL
entities = []

types = typeEnvFromList
    []

funcons = libFromList
    [("stuck",NullaryFuncon stepStuck)]

stuck_ = FName "stuck"
stepStuck = norule (FName "stuck")