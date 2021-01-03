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
## RandomString
##===========================================================

#' Random strings
#' @description Generates a vector of random strings.
#' @param size Number of words.
#' @param lambda Poisson lambda for the string length distribution.
#' @param charClasses Character vector specifying character classes to draw elements from.
#' The known classes are \code{c("[A-Z]", "[a-z]", "[0-9]")}.
#' If NULL it is the same as \code{ c("[A-Z]", "[a-z]")}.
#' @export
RandomString <- function( size = 1, lambda = 4, charClasses = c("[A-Z]", "[a-z]") ) {

  if( size < 1 ) {
    stop("The argument n is expected to be a positive integer.", call. = TRUE)
  }

  if( lambda < 1 ) {
    stop("The argument lambda is expected to be a positive integer.", call. = TRUE)
  }

  if( !( is.character(charClasses) || is.null(charClasses) ) ) {
    stop("The argument charClasses is expected to be a character vector or NULL.", call. = TRUE)
  }

  if( is.null(charClasses) || length(charClasses) == 1 && charClasses == "[A-Za-z]") {
    charClasses <- c("[A-Z]", "[a-z]")
  } else if ( length(charClasses) == 1 && charClasses == "[A-Za-z0-9]") {
    charClasses <- c("[A-Z]", "[a-z]", "[0-9]")
  }

  lsAll <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", "")[[1]]
  lsAZ <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "")[[1]]
  lsaz <- strsplit("abcdefghijklmnopqrstuvwxyz", "")[[1]]
  ls09 <- strsplit("0123456789", "")[[1]]

  lsChars <-
    Reduce(
      f = function( a, s) {
        if( s == "[A-Z]" ) {
          c( a, lsAZ)
        } else if ( s == "[a-z]" ) {
          c( a, lsaz)
        } else if ( s == "[0-9]" ) {
          c( a, ls09)
        } else {
          c( a, strsplit(s,"")[[1]])
        }
      },
      init = c(),
      x = charClasses
    )
  lsChars <- lsChars[nchar(lsChars) > 0]
  purrr::map_chr( 1:size,  ~ paste( sample( x = lsChars, size = rpois( n = 1, lambda = lambda - 1 ) + 1, replace = T), collapse = "") )
}


##===========================================================
## RandomWord
##===========================================================

#' Random words
#' @description Generates a vector of random words.
#' @param size Number of words.
#' @param type Type of words.
#' One of \code{c("Any", "Known", "Common", "Stopword")}.
#' If NULL is the same as "Any".
#' @export
RandomWord <- function( size = 1, type = "Any") {

  if( size < 1 ) {
    stop( "The argument n is expected to be a positive integer.", call. = TRUE)
  }

  lsExpectedTypes <- c("Any", "Known", "Common", "Stopword")
  if( is.null(type) ) { type  <- "Any" }
  if( !( is.character(type) && tolower(type) %in% tolower(lsExpectedTypes) ) ) {
    stop( paste( "The argument type is expected to be NULL or one of", lsExpectedTypes ), call. = TRUE)
  }
  type <- tolower(type)

  if( type == "any" ) {
    dfRes <- dfEnglishWords
  } else if ( type == "common" ) {
    dfRes <- dfEnglishWords %>% dplyr::filter( CommonWordQ )
  } else if ( type == "known" ) {
    dfRes <- dfEnglishWords %>% dplyr::filter( KnownWordQ )
  } else if ( type == "stopword" ) {
    dfRes <- dfEnglishWords %>% dplyr::filter( StopWordQ )
  } else {
    stop( paste("Uknown type", type, "."), call. = TRUE)
  }

  dfRes %>% dplyr::sample_n( size = size ) %>% dplyr::pull( Word )
}


##===========================================================
## RandomDate
##===========================================================

#' Random dates
#' @description Generates a vector of random POSIXct dates.
#' @param size Number of words.
#' @param min Minimum date.
#' If NULL then it same as the \code{origin}.
#' @param max Maximum date.
#' If NULL then it is the same as "2100-01-01".
#' @param ... Additional arguments for \code{\link{as.POSIXct}}
#' @details If numerical values are given to the arguments
#' \code{min} and \code{max} then an \code{origin} argument has to be supplied.
#' @export
RandomDate <- function( size = 1, min = NULL, max = NULL, ... ) {

  minDate <- min
  maxDate <- max

  if( is.null(minDate) ) { minDate <- "1900-01-01"}
  if( is.null(maxDate) ) { maxDate <- "2100-01-01"}

  if( is.numeric(minDate) || is.character(minDate) || class(minDate) == "Date") {
    minDate <- as.POSIXct( x = minDate, ... )
  }

  if( is.numeric(maxDate) || is.character(maxDate) || class(maxDate) == "Date" ) {
    maxDate <- as.POSIXct( x = maxDate, ... )
  }

  if( !("POSIXct" %in% class(minDate)) ) {
    stop( "The argument min is expected to be a POSIXct object, or a date string, or a number.", call. = TRUE )
  }

  if( !("POSIXct" %in% class(maxDate)) ) {
    stop( "The argument max is expected to be a POSIXct object, or a date string, or a number.", call. = TRUE )
  }


  timeIterval <- as.numeric(maxDate) - as.numeric(minDate)

  res <- runif( n = size, min = 0, max = timeIterval )

  minDate + res
}
