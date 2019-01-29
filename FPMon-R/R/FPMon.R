#' FPMon: a package sequential construction of parsers
#' Anton Antonov
#' Version 0.7
#' 
#' @section In brief
#' This R package provides an implementation of a system of functional parsers. 
#' The implementation follows closely the article:
#'   
#'   "Functional parsers" by Jeroen Fokker .
#' 
#' The script also follows closely the implementation in the Mathematica package FunctionalParsers.m:
#'
#' [1] Anton Antonov, Functional parsers Mathematica package,
#'     https://github.com/antononcube/MathematicaForPrediction/blob/master/FunctionalParsers.m , (2014).
#'
#' @section Structure
#' The parsers are categorized in the groups: basic, combinators, and transformers.
#' The basic parsers parse specified strings and strings adhering to predicates.
#' The combinator parsers allow sequential and alternative combinations of parsers.
#' The transformer parsers change the input or the output of the parsers that are transformed.
#' 
#' A basic or a combinator parser takes a list of strings and returns a list of pairs, {{o1,r1},{o2,r2},...}. 
#' Each pair has as elements a parsed output and the rest of the input list.
#'
#' The parsers have long descriptive names. Short names are programmed for convenience.
#' For example, "p.apply" is the short name of "ParseApply".
#' "p.seql" is the short name of "ParseSequentialCompositionPickLeft".
#' For some parsers there are infix operators. For example, "@" can be used for "ParseApply".
#' See the sections "Shorter parser names" and "Infix notation" in FunctionalParsers.R .
#' 
#' @section Miscellaneous
#' There is a plan the package also have functions to generate parsers from a string of the
#' Extended Backus-Naur Form (EBNF) definition of a grammar.
#' 
#' This version of the R functions / package does not have functions for parser 
#' generation from of EBNF specification (which are implemented in the Mathematica 
#' package FunctionalParsers.m .)
#' 
#' This preliminary release is put on GitHub because I developed R functions 
#' 4.5 years ago (end of December 2014), I want to start a process of reviewing
#' and completing the functionality and creating an actual R package.
#' 
#' @docType package
#' @name FPMon
NULL
