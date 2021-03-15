{-# LANGUAGE OverloadedStrings #-}

module Funcons.Core.Values.Primitive.CharactersBuiltin where

import Funcons.EDSL
import qualified Funcons.Operations as VAL

library = libFromList [
        ("characters", fromNullaryValOp characters_ VAL.characters_)
    ,   ("chars", fromNullaryValOp characters_ VAL.characters_)
    ,   ("unicode-characters", fromNullaryValOp unicode_characters_ VAL.unicode_characters_)
    ,   ("unicode-chars", fromNullaryValOp unicode_characters_ VAL.unicode_characters_)
    ,   ("unicode-points", fromNullaryValOp unicode_points_ VAL.unicode_points_)
    ,   ("basic-multilingual-plane-points", fromNullaryValOp basic_multilingual_plane_points_ VAL.bmp_points_)
    ,   ("unicode-character", fromValOp unicode_character_ VAL.unicode_character_)
    ,   ("unicode-char", fromValOp unicode_character_ VAL.unicode_character_)
    ,   ("basic-multilingual-plane-characters", fromNullaryValOp basic_multilingual_plane_characters_ VAL.bmp_characters_)
    ,   ("bmp-chars", fromNullaryValOp basic_multilingual_plane_characters_ VAL.bmp_characters_)
    ,   ("iso-latin-1-characters", fromNullaryValOp iso_latin_1_characters_ VAL.iso_latin_characters_)
    ,   ("latin-1-chars", fromNullaryValOp iso_latin_1_characters_ VAL.iso_latin_characters_)
    ,   ("ascii-characters", fromNullaryValOp ascii_characters_ VAL.ascii_characters_)
    ,   ("ascii-chars", fromNullaryValOp ascii_characters_ VAL.ascii_characters_)
    ]

ascii_characters_ = applyFuncon "ascii-characters"
ascii_chars_ = applyFuncon "ascii-characters"
unicode_characters_ = applyFuncon "unicode-characters"
iso_latin_1_characters_ = applyFuncon "iso-latin-1-characters"
latin_1_chars_ = applyFuncon "iso-latin-1-characters"
basic_multilingual_plane_characters_ = applyFuncon "basic-multilingual-plane-characters"
bmp_chars_ = applyFuncon "basic-multilingual-plane-characters"
unicode_points_ = applyFuncon "unicode-points"
basic_multilingual_plane_points_ = applyFuncon "basic-multilingual-plane-points"
ascii_character_ = applyFuncon "ascii-character"
unicode_character_ = applyFuncon "unicode-character"
unicode_char_ = applyFuncon "unicode-character"
--unicode_point_ = applyFuncon "unicode-point" TODO reveal once built-in
--unicode_ = applyFuncon "unicode-point" TODO reveal once built-in
characters_ = applyFuncon "characters"
chars_ = applyFuncon "characters"
