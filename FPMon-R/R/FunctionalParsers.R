##================================================================================
##  Functional parsers in R
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
##================================================================================

## The dependency from 'plyr' can be probably eleminated.
library('plyr')


##============================================================
## Basic parsers
##============================================================

#' Parsing a "symbol".
#' @description Gives a parser for a given string.
#' @param a A string.
#' @return A function that is a parser.
#' @family basic parsers
#' @export
ParseSymbol <- function(a) {
  force(a)
  function(xs) {
    ## cat("ParseSymbol::xs="); print(xs)
    ## cat("ParseSymbol::a="); print(a)
    if( length(xs) > 0 && xs[1] == a ) { list( list( stream=xs[-1], parsed=c(a) ) ) } else { NULL }
  }
}

#' Parsing with a predicate.
#' @description Gives a parser for a given predicate.
#' @param predFunc A function that is a certain predicate.
#' @return A function that is a parser. 
#' @family basic parsers
#' @export
ParsePredicate <- function(predFunc) {
  force(predFunc)
  function(xs) {
    ##cat( "xs[1]=", xs[1], " predFunc(xs[1])=", predFunc(xs[1]), "\n" )
    if( length(xs) > 0 && predFunc(xs[1]) ) { list( list( stream=xs[-1], parsed=c(xs[1]) ) ) } else { NULL }
  }
}

#' Parser that does nothing.
#' @description Gives a parser that parses nothing.
#' @return A function that is a parser.
#' @family basic parsers
#' @export
ParseEpsilon <- function(xs) { list( list( stream=xs, parsed=NULL ) ) }

#' Parser that succeeds.
#' @description Gives a parser that does a successful parsing.
#' @param v A "symbol" that is the successful parsing result.
#' @return A function that is a parser. 
#' @family basic parsers
#' @export
ParseSucceed <- function(v) { force(v); function(xs) { list( list( stream=xs, parsed=v ) ) } }

#' Parser that fails.
#' @description Gives a parser that returns NULL.
#' @return A function that is a parser.
#' @family basic parsers
#' @export
ParseFailed <- function(xs) {NULL}


##============================================================
## Parser combinators
##============================================================

#' Composer of parsing results.
#' @description Gives a list of the results of a parser applied over a list.
#' @param p A parser to be applied to the parsing results.
#' @param res A list of parsing results.
#' @return A list of parsing results.
#' @export
ParseComposeWithResults <- function(p, res) {
  ##cat("ParseComposeWithResults::res="); print(res); cat("\n")
  if ( length(res) == 0 ) {
    NULL 
  } else {
    pRes <-
      plyr::llply( res,
             function(r) {
               if ( is.null(r) ) { NULL } else {
                 ##cat("ParseComposeWithResults::r= "); print(r)
                 pr <- p( r$stream )
                 ##cat("ParseComposeWithResults::pr= "); print(pr)
                 if ( is.null( pr ) || is.na(pr) || length( pr ) == 0 ) { NULL } else {
                   plyr::llply( pr, function(x) list( stream = x$stream, parsed = list( r$parsed, x$parsed ) ) )
                 }
               }   
             })
    ##pRes <- Filter(Negate(is.null), pRes)
    pRes <- pRes[ !sapply(pRes, function(x) is.null(x) || is.na(x) ) ]
    ##cat("ParseComposeWithResults::pRes= "); print(ListToString(pRes))
    if ( length(pRes) == 0 ) { NULL }
    else {
      ##cat("ParseComposeWithResults::unlist(pRes)= "); print(ListToString(unlist( pRes, recursive = FALSE )))
      unlist( pRes, recursive = FALSE )
    }
  }
}

#' Compose parsers for sequential parsing.
#' @description Gives a parser that is a sequential composition of parsers.
#' @param parsers A list of parsers.
#' @return A function that is a parser. 
#' @details A basic combinator of parsers. Note the argument is a list of parsers.
#'   A (probably) better version would take "just" arguments.
#'   \code{\alias{\%&\%}} is an infix version;
#' @seealso \code{\link{p.seq}} is a shorter synonym;
#'   \code{\link{ParseAlternativeComposition}} for alternating composition;
#'   \code{\link{ParseComposeWithResults}} for how the results are obtained.
#' @family basic combinators
#' @export
ParseSequentialComposition <- function( parsers ) {
  force(parsers)
  ##if( exists('fclosure') ) { fclosure$parsers <- parsers }
  if ( class(parsers) == "function" ) { function(xs) parsers(xs) }
  else if ( class(parsers) == "list" && length(parsers) == 1 ) { function(xs) parsers[[1]](xs) }
  else {
    function(xs) {
      ##cat("ParseSequentialComposition::parsers[[1]](xs)="); print( parsers[[1]](xs) )
      res <- Reduce( function(x,y) { ParseComposeWithResults(y, x) }, parsers[-1], parsers[[1]](xs) )
      ##cat("ParseSequentialComposition::res="); print(res)
      res
    }
  }
}

#' Compose parsers for alternate parsing.
#' @description Gives a parser that is an alternating composition of parsers.
#' @param parsers A list of parsers.
#' @return A function that is a parser. 
#' @details A basic combinator of parsers. Note the argument is a list of parsers.
#'   A (probably) better version would take "just" arguments.
#'   \alias{\%|\%}} is an infix version.
#' @seealso \code{\link{p.alt}} is a shorter synonym;
#'   \code{\link{ParseSequentialComposition}} for sequential composition.
#' @family basic combinators
#' @export
ParseAlternativeComposition <- function( parsers ) {
  force(parsers)
  function(xs) {
    pRes <- plyr::llply( parsers, function(f) f(xs) )
    pRes <- pRes[ !sapply(pRes, function(x) is.null(x) || is.na(x) || length(pRes)==0 ) ]
    ##cat("ParseAlternativeComposition::res= "); print(ListToString( pRes ) )
    unlist( pRes, recursive=FALSE )
  }
}

##============================================================
## Next combinators
##============================================================

#' Take parsing results with no residuals.
#' @description Gives a parser that takes only completely successful results.
#' @param p A parser.
#' @return A function that is a parser. 
#' @family next combinators
#' @export
ParseJust <- function(p) {
  force(p)
  function(xs) { Filter( function(x) is.null( x$stream ) || length(x$stream) == 0, p(xs) ) }
}

#' Apply a function to parsing results.
#' @description Gives a parser that modifies the parsing results with specified function.
#' @param f A function to applied to the parsing results.
#' @param p A parser.
#' @return A function that is a parser. 
#' @family next combinators
#' @export
ParseApply <- function(f, p) {
  force(f); force(p)
  function(xs) { plyr::llply( p(xs), function(x) list( stream=x$stream, parsed=f(x$parsed) ) ) }
}

#' Take the first completely successful parsing result.
#' @description Gives a parser that takes the first result of \code{\link{ParseJust}}.
#' @param p A parser.
#' @return A function that is a parser. 
#' @seealso \code{\link{ParseJust}} has similar functionality.
#' @family next combinators
#' @export
ParseSome <- function(p) {
  force(p) 
  function(xs) {
    pres <- ParseJust(p)(xs)
    if ( length(pres) > 0 ) { pres[1] } else { pres }
  }
}

#' Take the parsing result with the shortest residual.
#' @description Gives a parser that takes the result with the shortest residual.
#' @param p A parser.
#' @return A function that is a parser. 
#' @family next combinators
#' @export
ParseShortest <- function(p) {
  force(p)
  function(xs) {
    pres <- p(xs)     
    if ( is.null(pres) ) { NULL }
    else {
      pres <- pres[ plyr::laply( pres, function(x) !is.null(x) ) ]
      ##cat("ParseShortest::pres=");print(pres)
      if ( length(pres) > 0 ) {
        lens <- plyr::laply( pres, function(x) length(x$stream) )
        ##cat("ParseShortest::lens=");print(lens)
        pres[ order( lens ) ][1]
      } else {
        NULL
      }
    }
  }
}

#' Take the left parsing result.
#' @description Gives a parser that takes the left parsing result 
#'   of a sequential composition of two parsers.
#' @param p A parser.
#' @return A function that is a parser. 
#' @seealso \code{\link{ParseSequentialComposition}} is the generic function;
#'   \code{\link{ParseSequentialCompositionPickRight}} for picking the right result.
#' @details Note that the function takes two parsers not a list of parsers
#'   as \code{\link{ParseSequentialComposition}}.
#' @family next combinators
#' @export
ParseSequentialCompositionPickLeft <- function( p1, p2 ) {
  force(p1); force(p2)
  function(xs) {
    ParseApply( function(x) x[[1]], ParseSequentialComposition( c( p1, p2 ) ) ) (xs)
  }
}

#' Take the right parsing result.
#' @description Gives a parser that takes the right parsing result 
#'   of a sequential composition of two parsers.
#' @param p A parser.
#' @return A function that is a parser. 
#' @seealso \code{\link{ParseSequentialComposition}} is the generic function;
#'   \code{\link{ParseSequentialCompositionPickLeft}} for picking the left result.
#' @details Note that the function takes two parsers not a list of parsers
#'   as \code{\link{ParseSequentialComposition}}.
#' @family next combinators
#' @export
ParseSequentialCompositionPickRight <- function( p1, p2 ) {
  force(p1); force(p2)
  function(xs) {
    ParseApply( function(x) x[[2]], ParseSequentialComposition( c( p1, p2 ) ) ) (xs)
  }
}

ParseChoice <- function( ) {
  
}

##============================================================
## Second next combinators
##============================================================

#' Parsing a triplet-pack.
#' @description Used to parse rules that adhere to the pattern "separator1 expr separator2".
#' @param s1 A parser (left separator).
#' @param p A parser (middle).
#' @param s2 A parser (right separator).
#' @return A function that is a parser.
#' @family Pack parsers.
#' @export
ParsePack <- function( s1, p, s2 ) {
  force(s1); force(p); force(s2)
  ##print( ListToString( ParseSequentialCompositionPickRight( s1, p ) ) )
  ParseSequentialCompositionPickLeft( ParseSequentialCompositionPickRight( s1, p ), s2 )
}

#' Parsing token(s) between parentheses.
#' @description Used to parse rules that adhere to the pattern "( parser )".
#' @param p A parser of tokens between parentheses.
#' @return A function that is a parser.
#' @family Pack parsers.
#' @export
ParseParenthesized <- function( p ) { ParsePack ( ParseSymbol("("), p, ParseSymbol(")") ) }

#' Parsing token(s) between brackets.
#' @description Used to parse rules that adhere to the pattern "[ expr ]".
#' @param p A parser of tokens between brackets
#' @return A function that is a parser.
#' @family Pack parsers.
#' @export
ParseBracketed <- function( p ) { ParsePack ( ParseSymbol("["), p, ParseSymbol("]") ) }

#' Parsing token(s) between curly brackets.
#' @description Used to parse rules that adhere to the pattern "{ expr }".
#' @param p A parser of tokens between curly brackets
#' @return A function that is a parser.
#' @family Pack parsers.
#' @export
ParseCurlyBracketed <- function( p ) { ParsePack ( ParseSymbol("{"), p, ParseSymbol("}") ) }

#' Parsing optionally.
#' @description Used to parse optional presence.
#' @param p A parser.
#' @return A function that is a parser.
#' @family Next parsers.
#' @export
ParseOption <- function(p) {
  force(p)
  ParseAlternativeComposition( c( ParseApply( list, p ) , 
                                  ParseSucceed( list() )  ) )
}

#' Parsing optionally (second version).
#' @description Used to parse optional presence.
#' @param p A parser.
#' @return A function that is a parser.
#' @family Next parsers.
#' @export
ParseOption1 <- function(p) {
  force(p)
  function(xs) {
    res <- p(xs)
    if ( is.null(res) ) { list( list(stream=xs, parsed=NULL) ) } else { res }
  }
}

#' Parsing a sequence of many.
#' @description Used to parse a sequence of many patterns corresponding of a parser.
#' @param p A parser.
#' @return A function that is a parser.
#' @family Next parsers.
#' @export
ParseMany1 <- function(p) {
  force(p)
  function(xs) {
    oldStream <- xs
    accRes <- NULL
    res <- ParseShortest( ParseOption1( p ) ) ( xs )
    while ( !( is.null(res) || length(res) == 0 || is.null( res[[1]]$parsed ) ) ) {
      if ( is.null( accRes ) ) {
        accRes = c( res[[1]]$parsed )
      } else {
        accRes <- c( accRes, res[[1]]$parsed )
      }
      oldStream <- res[[1]]$stream
      res <- ParseShortest( ParseOption1( p ) ) ( res[[1]]$stream )
    }
    if ( is.null(accRes) ) { list( list( stream=xs, parsed=NULL ) ) }
    else { list( list( stream=oldStream, parsed=accRes ) ) }
  }
}

#' Parsing a sequence of many.
#' @description Used to parse a sequence of many patterns corresponding of a parser.
#' @param p A parser.
#' @return A function that is a parser.
#' @family Next parsers.
#' @export
ParseMany <- function(p) { ParseAlternativeComposition( c( ParseMany1(p), ParseSucceed(NULL) ) ) }

## I am not sure that ParseApply is needed
#' Parsing a list of many.
#' @description Used to parse a sequence of many patterns corresponding of a parser 
#'   separated by patterns corresponding to a second parser.
#' @param p A parser.
#' @param sepParser A separator parser.
#' @return A function that is a parser.
#' @family Next parsers.
#' @export
ParseListOf <- function( p, sepParser ) {
  force(p); force(sepParser)
  
  ParseAlternativeComposition( c(
    ParseApply(
      function(x) c( list( x[[1]] ), x[[2]] ),
      ParseSequentialComposition( c( p,
                                     ParseMany1( ParseSequentialCompositionPickRight( sepParser, p ) ) )
      ) ),
    ParseSucceed( NULL ) ) )
}

#' Parsing a chain.
#' @description Parsing a chain over a certain operation.
#' @param p A parser.
#' @param sepParser A separator parser.
#' @param right A logical; gives the direction in which the chain is constructed.
#' @return A function that is a parser.
#' @family Next parsers.
#' @export
ParseChain <- function( p, sepParser, right = FALSE ) {
  force(p); force(sepParser); force(right)
  ParseAlternativeComposition( c(
    ParseApply( 
      function(x) {
        if ( length(x)==1 ) { 
          x 
        } else if ( is.null(x[[2]]) ) {
          x[1]
        } else { ## at this point length(x) >= 3 and length(x) == 2*k-1
          ## op <- x[[2]][[1]]
          ops <-x[[2]][ seq( 1, length(x[[2]]), 2 ) ]
          elems <- c(x[[1]], x[[2]][ seq( 2, length(x[[2]]), 2 ) ] )
          if ( right ) {
            ## Reduce( f = function(e,a) list(op, a, e[[1]]), x = elems[-length(elems)], init = elems[[length(elems)]], right = right ) 
            Reduce( f = function(e,a) list(ops[[e]], a, elems[[e]]), x = 1:(length(elems)-1), init = elems[[length(elems)]], right = right ) 
          } else {
            ## Reduce( f = function(a,e) list(op, a, e[[1]]), x = elems[-1], init = elems[[1]], right = right ) 
            Reduce( f = function(a,e) list(ops[[e-1]], a, elems[[e]]), x = 2:length(elems), init = elems[[1]], right = right ) 
          }
        }
      },
      ParseSequentialComposition( c( p,
                                     ParseMany( ParseSequentialComposition( c( sepParser, p ) ) )
      )
      )
    ),
    ParseSucceed( NULL ) ) )
}

#' Left parsing a chain.
#' @description Parsing a chain over a certain operation.
#' @param p A parser.
#' @param sepParser A separator parser.
#' @return A function that is a parser.
#' @family Next parsers.
#' @export
ParseChainLeft <- function( p, sp ) ParseChain( p, sp, right=FALSE )

#' Right parsing a chain.
#' @description Parsing a chain over a certain operation.
#' @param p A parser.
#' @param sepParser A separator parser.
#' @return A function that is a parser.
#' @family Next parsers.
#' @export
ParseChainRight <- function( p, sp ) ParseChain( p, sp, right=TRUE )


##============================================================
## Infix notation
##============================================================

#' Infix function for ParseSequentialComposition. 
#' @export
'%&%' <- function( p1, p2 ) { ParseSequentialComposition( c( p1, p2 ) ) }

#' Infix function for ParseSequentialCompositionPickLeft.
#' @export
'%<&%' <- function( p1, p2 ) { ParseSequentialCompositionPickLeft( p1, p2 ) }

#' Infix function for ParseSequentialCompositionPickRight.
#' @export
'%&>%' <- function( p1, p2 ) { ParseSequentialCompositionPickRight( p1, p2 ) }

#' Infix function for ParseAlternativeComposition. 
#' @export
'%|%' <- function( p1, p2 ) { ParseAlternativeComposition( c( p1, p2 ) ) }

#' Infix function for ParseApply. 
#' @export
'%@%' <- function( p1, p2 ) { ParseApply( p1, p2 ) }

#' Infix function for ParseApply. 
#' @export
'%@>%' <- '%@%'

#' Postfix form function for ParseApply. 
#' @export
'%<@%' <- function( p1, p2 ) { ParseApply( p2, p1 ) }


##============================================================
## Shorter parser names
##============================================================

#' Synonym of \code{ParseSymbol}.
#' @seealso \code{\link{ParseSymbol}} is the original function.
#' @family "short names" parsers
#' @export
p.symbol <- ParseSymbol

#' Synonym of \code{ParsePredicate}.
#' @seealso \code{\link{ParsePredicate}} is the original function.
#' @family "short names" parsers
#' @export
p.satisfy <- ParsePredicate

#' Synonym of \code{ParsePredicate}.
#' @seealso \code{\link{ParsePredicate}} is the original function.
#' @family "short names" parsers
#' @export
p.pred <- ParsePredicate

#' Synonym of \code{ParseFailed}.
#' @seealso \code{\link{ParseFailed}} is the original function.
#' @family "short names" parsers
#' @export
p.fail <- ParseFailed

#' Synonym of \code{ParseSequentialComposition}.
#' @seealso \code{\link{ParseSequentialComposition}} is the original function.
#' @family "short names" parsers
#' @export
p.seq <- ParseSequentialComposition

#' Synonym of \code{ParseAlternativeComposition}.
#' @seealso \code{\link{ParseAlternativeComposition}} is the original function.
#' @family "short names" parsers
#' @export
p.alt <- ParseAlternativeComposition

#' Synonym of \code{ParseJust}.
#' @seealso \code{\link{ParseJust}} is the original function.
#' @family "short names" parsers
#' @export
p.just <- ParseJust

#' Synonym of \code{ParseApply}.
#' @seealso \code{\link{ParseApply}} is the original function.
#' @family "short names" parsers
#' @export
p.apply <- ParseApply

#' Synonym of \code{ParseSome}.
#' @seealso \code{\link{ParseSome}} is the original function.
#' @family "short names" parsers
#' @export
p.some <- ParseSome

#' Synonym of \code{ParseShortest}.
#' @seealso \code{\link{ParseShortest}} is the original function.
#' @family "short names" parsers
#' @export
p.shortest <- ParseShortest

#' Synonym of \code{ParseSequentialCompositionPickLeft}.
#' @seealso \code{\link{ParseSequentialCompositionPickLeft}} is the original function.
#' @family "short names" parsers
#' @export
p.seql <- ParseSequentialCompositionPickLeft

#' Synonym of \code{ParseSequentialCompositionPickRight}.
#' @seealso \code{\link{ParseSequentialCompositionPickRight}} is the original function.
#' @family "short names" parsers
#' @export
p.seqr <- ParseSequentialCompositionPickRight

#' Synonym of \code{ParsePack}.
#' @seealso \code{\link{ParsePack}} is the original function.
#' @family "short names" parsers
#' @export
p.pack <- ParsePack

#' Synonym of \code{ParseParenthesized}.
#' @seealso \code{\link{ParseParenthesized}} is the original function.
#' @family "short names" parsers
#' @export
p.parenthesized <- ParseParenthesized

#' Synonym of \code{ParseBracketed}.
#' @seealso \code{\link{ParseBracketed}} is the original function.
#' @family "short names" parsers
#' @export
p.bracketed <- ParseBracketed

#' Synonym of \code{ParseCurlyBracketed}.
#' @seealso \code{\link{ParseCurlyBracketed}} is the original function.
#' @family "short names" parsers
#' @export
p.curlybracketed <- ParseCurlyBracketed

#' Synonym of \code{ParseOption}.
#' @seealso \code{\link{ParseOption}} is the original function.
#' @family "short names" parsers
#' @export
p.option <- ParseOption

#' Synonym of \code{ParseMany}.
#' @seealso \code{\link{ParseMany}} is the original function.
#' @family "short names" parsers
#' @export
p.many <- ParseMany

#' Synonym of \code{ParseMany1}.
#' @seealso \code{\link{ParseMany1}} is the original function.
#' @family "short names" parsers
#' @export
p.many1 <- ParseMany1

#' Synonym of \code{ParseListOf}.
#' @seealso \code{\link{ParseListOf}} is the original function.
#' @family "short names" parsers
#' @export
p.listof <- ParseListOf

#' Synonym of \code{ParseChainLeft}.
#' @seealso \code{\link{ParseChainLeft}} is the original function.
#' @family "short names" parsers
#' @export
p.chainl <- ParseChainLeft

#' Synonym of \code{ParseChainRight}.
#' @seealso \code{\link{ParseChainRight}} is the original function.
#' @family "short names" parsers
#' @export
p.chainr <- ParseChainRight

