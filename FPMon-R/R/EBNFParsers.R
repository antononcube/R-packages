##================================================================================
##  EBNF parsers in R
##
##  BSD 3-Clause License
##  
##  Copyright (c) 2015, Anton Antonov
##  All rights reserved.
##  
##  Redistribution and use in source and binary forms, with or without
##  modification, are permitted provided that the following conditions are met:
##    
##  * Redistributions of source code must retain the above copyright notice, this
##  list of conditions and the following disclaimer.
##  
##  * Redistributions in binary form must reproduce the above copyright notice,
##  this list of conditions and the following disclaimer in the documentation
##  and/or other materials provided with the distribution.
##  
##  * Neither the name of the copyright holder nor the names of its
##  contributors may be used to endorse or promote products derived from
##  this software without specific prior written permission.
##  
##  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
##  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
##  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
##  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
##  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
##  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
##  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
##  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
##  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
##  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
##  
##  Written by Anton Antonov, 
##  antononcube @ gmail . com,
##  Windermere, Florida, USA.
##  

source("./R/FunctionalParsers.R")

##============================================================
##  EBNF Parsers with parenthesis, <& and &>
##============================================================
##  All parsers start with the prefix "pG" followed by a capital letter. 
##  ("p" is for "parser", "G" is for "grammar".)


EBNFSymbolTest <- function(x) { is.character(x) && ( x == "|" || x == "," || x == "=" || x == ";" || x == "&>" || x == "<&" || x == "<@" ) }

NonTerminalTest <- function(x) grepl( pattern = "^<(\\w|\\s|\\-|\\_)*>$", x = x )

InQuotesTest <- function(x) grepl( pattern = "^(\'|\")(.*)(\'|\")$", x = x )

# pGTerminal <- p.pred( function(x) ( is.character(x) && InQuotesTest(x) && ! EBNFSymbolTest(x) ) )
pGTerminal <- p.apply( function(x) paste( unlist(x), collapse="" ), p.seq( c( p.symbol("'"), p.many1( p.pred( function(x) grepl( pattern = "^(\\w|\\s|\\-)$", x = x ) ) ), p.symbol("'") ) ) )

# pGNonTerminal <- p.pred( function(x) ( is.character(x) && NonTerminalTest(x) && ! EBNFSymbolTest(x) ) ) )
pGNonTerminal <- p.apply( function(x) paste( unlist(x), collapse="" ), p.seq( c( p.symbol("<"), p.many1( p.pred( function(x) grepl( pattern = "^(\\w|\\s|\\-)$", x = x ) ) ), p.symbol(">") ) ) )

pGRCode <- p.apply( function(x) paste( x, collapse="" ), p.many1( p.pred( function(x) grepl( pattern = "[^;]", x = x ) ) ) )


##pGNode[xs_] := (EBNFTerminal<@>pGTerminal<+>EBNFNonTerminal<@>pGNonTerminal<+>ParseParenthesized[pGExpr]<+>pGRepetition<+>pGOption)[xs];

EBNFTerminal <- function(x) ListToString( x, head="EBNFTerminal")
EBNFNonTerminal <- function(x) ListToString( x, head="EBNFNonTerminal")
pGNode <- function(xs) p.alt( c( p.apply( EBNFTerminal, pGTerminal), 
                                 p.apply( EBNFNonTerminal, pGNonTerminal ), 
                                 p.parenthesized( pGExpr ),
                                 pGRepetition, 
                                 pGOption ) ) (xs)

##pGTerm = EBNFSequence<@>ParseChainRight[pGNode, ParseSymbol[","]<+>ParseSymbol["<&"]<+>ParseSymbol["&>"]];

EBNFSequence <- function(x) ListToString( x, head="EBNFSequence")
# pGTerm <- p.apply( EBNFSequence, p.chainr( pGNode, p.alt( c( p.symbol(","), p.symbol("<&"), p.symbol("&>") ) ) ) )
pGTerm <- EBNFSequence %@% p.chainr( pGNode, p.alt( c( p.symbol(","), p.symbol("&") %&% p.symbol(">"), p.symbol("<") %&% p.symbol("&") ) ) )


##pGExpr = EBNFAlternatives<@>ParseListOf[pGTerm, ParseSymbol["|"]];

EBNFAlternatives <- function(x) ListToString( x, head="EBNFAlternatives")
pGExpr <- EBNFAlternatives %@% p.listof( pGTerm, p.symbol("|") )

EBNFOption <- function(x) ListToString( x, head="EBNFOption")
pGOption <- EBNFOption %@% ParseBracketed( pGExpr )

EBNFRepetition <- function(x) ListToString( x, head="EBNFRepetition")
pGRepetition <- EBNFRepetition %@% ParseCurlyBracketed( pGExpr )

##pGRule = EBNFRule<@>(pGNonTerminal<*>(ParseSymbol["="] &> pGExpr)<*>(ParseSymbol[";"]<+>(ParseSymbol["<@"]<*>ParsePredicate[StringQ[#] &] <& ParseSymbol[";"])));

EBNFRule <- function(x) ListToString( x, head="EBNFRule")
pGRule <- p.apply( EBNFRule, p.seq( c( pGNonTerminal, 
                                       p.seqr( p.symbol("="), pGExpr ), 
                                       p.alt( c( p.symbol(";"), 
                                                 p.seq( c( p.symbol("<"), p.symbol("@"), pGRCode, p.symbol(";") ) ) ) 
                                       ) ) ) )

##pEBNF = EBNF<@>ParseMany1[pGRule];

EBNF <- function(x) ListToString( x, head="EBNF" )
pEBNF <- EBNF %@% p.many1( pGRule )


##============================================================
## Grammar parser generators with <& and &>
##============================================================

EBNFMakeSymbolName <- function(p) paste( "p" , toupper( gsub( pattern="\\<|\\>|\\_|\\-", replacement = "", p) ), sep="", collapse="" )


