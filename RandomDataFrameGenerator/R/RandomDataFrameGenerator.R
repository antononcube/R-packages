##===========================================================
## Random Data Frame Generator in R
##
## BSD 3-Clause License
##
## Copyright (c) 2020, Anton Antonov
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
##
## * Redistributions of source code must retain the above copyright notice, this
## list of conditions and the following disclaimer.
##
## * Redistributions in binary form must reproduce the above copyright notice,
## this list of conditions and the following disclaimer in the documentation
## and/or other materials provided with the distribution.
##
## * Neither the name of the copyright holder nor the names of its
## contributors may be used to endorse or promote products derived from
## this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
## IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
## FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
## DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
## SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
## CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
## OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
## OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
## Written by Anton Antonov,
## antononcube @@@ posteo ... net,
## Windermere, Florida, USA.
##===========================================================

#' @import dplyr
#' @import magrittr
#' @import purrr
NULL


##===========================================================
## RandomDataFrame
##===========================================================

#' Random data frame
#' @description Generates a random data frame using different specifications.
#' @param nrow Number of rows.
#' If NULL a \code{rpois(1,20)} is used.
#' @param ncol Number of columns.
#' If NULL a \code{rpois(1,7)} is used.
#' Ignored if \code{columnNames} is not NULL.
#' @param columnNames Column names.
#' If NULL random strings are used.
#' @param columnNamesGenerator
#' @param generators
#' @param maxNumberOfValues Maximum number of non-NA values.
#' @param minNumberOfValues Minimum number of non-NA values.
#' @param rowNamesQ Should the generated data frame have row names or not?
#' @export
RandomDataFrame <- function( nrow = NULL, ncol = NULL,
                             columnNames = NULL,
                             columnNamesGenerator = NULL,
                             generators = NULL,
                             minNumberOfValues = NULL, maxNumberOfValues = NULL,
                             rowNamesQ = FALSE ) {


  ## Number of rows
  if( is.null(nrow) ) { nrow = rpois(n = 1, lambda = 20 ) }

  ## Number of columns
  lsColNames <- NULL
  if( is.null(columnNames) ) {
    if( is.null(ncol) ) { ncol = rpois(n = 1, lambda = 7) }
  } else if ( is.character(columnNames) ) {
    lsColNames <- columnNames
    ncol <- length(columnNames)
  } else {
    stop("The argument columnNames is expected to be a character vector or NULL.", call. = TRUE)
  }

  ## Column names generator
  if( is.null(lsColNames) ) {
    if ( is.null(columnNamesGenerator) ) {
      lsColNames <- RandomWord(size = ncol, type = "Common")
    } else if( is.function(columnNamesGenerator) ) {
      lsColNames <- columnNamesGenerator(ncol)
    } else if( is.na(columnNamesGenerator) ) {
      lsColNames <- as.character(seq(1, ncol, 1))
    }
  }

  ## Make sure column names are valid (do not have "-", do not start with a digit, etc.)
  ## Otherwise at the end we will get:
  ## Error in `[.data.frame`(dfRes, , lsColNames) : undefined columns selected
  #lsColNames <- gsub( pattern = "-", replacement = ".", x = lsColNames)
  lsColNames <- make.names(lsColNames)

  ## Generators
  aDefaultGenerators <-
    purrr::map( 1:ncol, function(i) {
      if( runif(1) <= 0.5) {
        function(k) RandomWord(size = k, type = "Any" )
      } else {
        function(k) rnorm(n = k, mean = 100, sd = 30 )
      }
    })
  names(aDefaultGenerators) <- lsColNames

  if( is.null(generators) ) {

    aGenerators <- aDefaultGenerators

  } else if( is.function(generators) ) {

    aGenerators <- rep_len( x = list(generators), length.out = ncol )
    names(aGenerators) <- lsColNames

  } else if( is.list(generators) && is.null( names(generators) ) ) {

    aGenerators <- rep_len( x = generators, length.out = ncol )
    names(aGenerators) <- lsColNames

  } else if( is.list(generators) && !is.null( names(generators) ) ) {

    aGenerators <- aDefaultGenerators
    lsCommonGenNames <- intersect( names(aGenerators), names(generators) )

    if( length(lsCommonGenNames) > 0 ) {
      aGenerators[lsCommonGenNames] <- generators[lsCommonGenNames]
    }

  } else {
    stop( "Unknown type of generators specificiation.", call. = TRUE )
  }

  ## Max Number Of Values
  if( is.null(maxNumberOfValues) ) {
    maxNumberOfValues <- nrow * ncol
  }
  if(maxNumberOfValues < 0 ) {
    stop("The argument maxNumberOfValues is expected to be a non-negative integer or NULL.", call. = TRUE)
  }

  ## Min Number Of Values
  if( is.null(minNumberOfValues) ) {
    minNumberOfValues <- maxNumberOfValues
  }
  if(maxNumberOfValues < 0 ) {
    stop("The argument minNumberOfValues is expected to be a non-negative integer or NULL.", call. = TRUE)
  }

  ## Generate coordinate pairs for the random values
  dfPairs <- setNames( expand.grid( 1:nrow, lsColNames, stringsAsFactors = FALSE ), c("Row", "Col") )

  if( maxNumberOfValues < nrow * ncol || minNumberOfValues < nrow * ncol ) {
    dfPairs <-
      dfPairs %>%
      dplyr::sample_n( size = runif( n = 1, min = minNumberOfValues, max = maxNumberOfValues ), replace = FALSE ) %>%
      dplyr::arrange( Row, Col ) %>%
      as.data.frame
  }

  if( is.null(dfPairs) || nrow(dfPairs) == 0 ) {
    dfRes <- do.call( data.frame, setNames( purrr::map( 1:ncol, ~ rep_len( x = NA, length.out = nrow) ), lsColNames ) )
    return(dfRes)
  }

  ## Generate random values
  dfRes <-
    purrr::map( split(dfPairs, dfPairs$Col), function(dfX) {
      if( nrow(dfX) == 0) {
        setNames( rep_len( x = NA, length.out = nrow), as.character(1:nrow) )
      } else {
        lsVals <- aGenerators[[ dfX$Col[[1]] ]]( nrow(dfX) )
        lsVals <- setNames( lsVals, as.character(dfX$Row) )
        lsVals <- setNames( lsVals[ as.character(1:nrow) ],  as.character(1:nrow) )
        lsVals
      }
    })


  ## Combine into final result
  dfRes <- do.call( data.frame, dfRes )
  dfRes <- dfRes[, lsColNames]

  ## With row names or not?
  if( !rowNamesQ ) {
    rownames(dfRes) <- NULL
  }

  dfRes
}
