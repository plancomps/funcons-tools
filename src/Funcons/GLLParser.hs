{-# LANGUAGE OverloadedStrings #-}

module Funcons.GLLParser where

import Funcons.Types
import GLL.Combinators hiding (many, some, Char, parse)

import Data.Char (isAlphaNum, isLower)
import Text.Regex.Applicative hiding ((<**>), optional)
import Data.Text (pack)
import Numeric

type Parser a = BNF Token a

fct_parse :: String -> Funcons
fct_parse = parser_a pFuncons

fct_parse_either :: String -> Either String Funcons
fct_parse_either s = case parsesWithErrors pFuncons s of
  Left err  -> Left err
  Right []  -> Left "no parse result"
  Right [f] -> Right f
  Right fs  -> Left "ambiguous parse result"

fvalue_parse :: String -> Funcons
fvalue_parse = FValue . fvalue_parse_ 

fvalue_parse_ :: String -> Values
fvalue_parse_ = parser_a pValues

parse :: Parser a -> String -> a
parse p str = case allParses p str of []    -> error "no parse"
                                      (a:_) -> a

parser_a :: Parser a -> String -> a
parser_a p string = case allParses p string of
  []    -> error "no parse"
  (f:_) -> f

allParses :: Parser a -> String -> [a]
allParses p string = GLL.Combinators.parseWithOptions [throwErrors] p 
                        (Funcons.GLLParser.lexer string) 

parsesWithErrors :: Parser a -> String -> Either String [a]
parsesWithErrors p string = GLL.Combinators.parseWithOptionsAndError [] p (Funcons.GLLParser.lexer string)

fct_lexerSettings = emptyLanguage {
    lineComment = "//"
  , identifiers = lName
  , keywords    = fct_keywords
  , keychars    = fct_keychars
  }

lexer = GLL.Combinators.lexer fct_lexerSettings

fct_keywords = ["void", "depends", "forall", "type_abs"
               ,"typevar", "?", "*", "+", "|->", "=>"]
fct_keychars = "{}(),'\"[]|^&~"

lName = (:) <$> psym isLower <*> many (psym (\c -> isAlphaNum c || c == '-'))

data FSuffix  = SuffixComputesFrom Funcons
              | SuffixSeq SeqSortOp
              | SuffixSortUnion Funcons
              | SuffixSortInter Funcons
              | SuffixPower Funcons

pFuncons :: Parser Funcons
pFuncons = "FUNCONS" 
  <:=  FSet               <$$> braces   (multipleSepBy pFuncons (keychar ','))
--  <||> FTuple             <$$> parens   (multipleSepBy pFuncons (keychar ','))
  <||> FApp "list"        <$$> brackets (multipleSepBy pFuncons (keychar ','))
  <||> FMap               <$$> braces   (multipleSepBy1 pKeyPair (keychar ','))
  <||> FSortComputes      <$$  keyword "=>" <**> pFuncons
  <||> FSortComplement    <$$  keychar '~' <**> pFuncons
  <||> suffix_select      <$$> pFuncons <**> pFSuffix 
  <||> suffix_select      <$$> parens pFuncons <**> pFSuffix 
  <||> maybe_apply . pack <$$> id_lit <**> optional pFunconss
  <||> FValue             <$$> pValues
 where
    maybe_apply nm Nothing = FName nm
    maybe_apply nm (Just (Right args)) = FApp nm args
    maybe_apply nm (Just (Left arg)) =  FApp nm [arg]

    suffix_select f1 s = case s of 
      SuffixComputesFrom f2 -> FSortComputesFrom f1 f2
      SuffixSeq op          -> FSortSeq f1 op
      SuffixSortUnion f2    -> FSortUnion f1 f2
      SuffixSortInter f2    -> FSortInter f1 f2
      SuffixPower f2        -> FSortPower f1 f2

    pFSuffix :: Parser FSuffix
    pFSuffix = "FSUFFIX" 
      <:=>  SuffixComputesFrom  <$$   keyword "=>" <**> pFuncons
      <||>  SuffixSeq           <$$>  pOp
      <||>  SuffixSortUnion     <$$   keychar '|' <**> pFuncons
      <||>  SuffixSortInter     <$$   keychar '&' <**> pFuncons
      <||>  SuffixPower         <$$   keychar '^' <**> pFuncons
                          
pFunconss :: Parser (Either Funcons [Funcons])
pFunconss = "FUNCONS-SEQUENCE" 
  <::=  Left  <$$> pFuncons
  <||>  Right . merge <$$> parens (multipleSepBy pFunconss (keychar ','))
  where merge = foldr op [] 
          where op (Left f) acc = f:acc
                op (Right fs) acc = fs++acc

pFunconsSeq :: Parser [Funcons]
pFunconsSeq = "FUNCONS-SEQ" 
  <:=> either (:[]) id <$$> pFunconss

pKeyPair :: Parser Funcons
pKeyPair = "KEYPAIR" <:=> 
 fBinding <$$> pFuncons <** keyword "|->" <**> pFunconss
  where fBinding k ev = FBinding k (either (:[]) id ev)

pOp :: Parser SeqSortOp
pOp = "OP" <:=> 
  StarOp  <$$  keyword "*"
          <||> PlusOp <$$ keyword "+"
          <||> QuestionMarkOp <$$ keyword "?"

pValues :: Parser Values
pValues = "VALUES" 
  <:=> mk_unicode_characters <$$> char_lit
  <||> string__  <$$> string_lit
  <||> mk_integers . toInteger <$$> int_lit 
  <||> IEEE_Float_64 . fst . head . readFloat <$$> pRatioAsString
 where  pRatioAsString = "RATIOasSTRING" -- NOT OK, would parse "-2.-3"
          <:=> (\m l -> show m ++ "." ++ show l) <$$> int_lit <** keychar '.'
                                                 <**> int_lit


