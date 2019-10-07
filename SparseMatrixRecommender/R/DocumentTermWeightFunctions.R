#=======================================================================================
# Implementation of document-term matrix re-weighting functions in R
# Copyright (C) 2014 Anton Antonov
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Written by Anton Antonov,
# antononcube @ gmail . com,
# Windermere, Florida, USA.
#

#=======================================================================================
# The developement of this code started with the intention that it will resemble the
# Mathematica package [1] as closely as possible, but the applications of functions
# per row or column is very slow in R, so only weight functions that are identified by
# strings are implemented, like, "IDF", "Cosine", etc.
#
# [1] Anton Antonov, Implementation of document-term matrix construction and re-weighting functions in Mathematica,
#     Mathematica package, DocumentTermMatrixConstruction.m at MathematicaForPrediction at GitHub , (2013).
#     URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/DocumentTermMatrixConstruction.m .
#
# History
# Started: September 2013
# Updated: May 2014, June 2014, July 2014, December 2014, January 2015.
##=======================================================================================
## 2014.12.17
## Implemented "Entropy". Fixed implementation of "Normal".
## Added "Binary" (same as "None").
##
##=======================================================================================
## 2015.01.12
## Implemented local weights. Changed the default values for the functons to be NULL.
##
##=======================================================================================
## 2016.02.11
## Implemented a function for making a document-term matrix from a collection of documents.
##
##=======================================================================================


#' @import Matrix
#' @import purrr
#' @import reshape2
NULL

# Defined for legacy code purposes
SMRApplyGlobalWeightFunction <- function( docTermMat, globalWeightFunction, normalizerFunction ) {
  SMRApplyTermWeightFunctions( docTermMat, globalWeightFunction, NULL, normalizerFunction )
}

#' Global weights for a document-term matrix.
#' @description Computes the global weights with a function specification.
#' @param docTermMat A document-term sparse matrix (dgCMatrix).
#' @param globalWeightFunction A global weight function ID.
#' @return Sparse matrix
#' @details  The implemented global weight function ID's are "IDF", "GFIDF", "Normal", "Binary", "Entropy", "Sum", "None".
#' @family Document-term matrix functions
#' @export
SMRGlobalTermFunctionWeights <- function( docTermMat, globalWeightFunction = "None" ) {
  
  if ( class(docTermMat) != "dgCMatrix" || nrow(docTermMat) < 1 || ncol(docTermMat) < 1 ) {
    stop( "The argument docTermMat is expected to be a sparse matrix with number of rows and columns greater than zero", call.=TRUE)
  }
  
  mat <- docTermMat
  
  if ( globalWeightFunction == "IDF" ) {
    
    # The following line seem to work, but gives messages. Using a direct access assignment instead.
    # mat[ mat>0 ] <- 1
    mat@x <- rep(1,length(mat@x))
    globalWeights <- colSums(mat)
    globalWeights[ globalWeights == 0 ] <- 1
    globalWeights <- log( nrow(mat) / globalWeights )
    
    # restore the original matrix
    mat <- docTermMat
    
  } else if ( globalWeightFunction == "GFIDF" ) {
    
    freqSums <- colSums(mat)
    mat@x <- rep(1,length(mat@x))
    globalWeights <- colSums(mat)
    globalWeights[ globalWeights == 0 ] <- 1
    globalWeights <- freqSums / globalWeights
    
    # restore the original matrix
    mat <- docTermMat
    
  } else if ( globalWeightFunction == "Normal" ) {
    
    globalWeights <- sqrt( colSums( mat*mat ) )
    globalWeights[ globalWeights == 0 ] <- 1
    globalWeights <- 1 / globalWeights
    
  } else if ( globalWeightFunction == "Binary" || globalWeightFunction == "None" ) {
    
    globalWeights <- rep(1, ncol(mat) )
    
  } else if ( globalWeightFunction == "ColumnStochastic" || globalWeightFunction == "Sum" ) {
    
    globalWeights <- colSums(mat)
    globalWeights[ globalWeights == 0 ] <- 1
    globalWeights <- 1 / globalWeights
    
  } else if ( globalWeightFunction == "Entropy" ) {
    
    gfs <- colSums(mat)
    gfs[ gfs == 0 ] <- 1
    pmat <- mat %*% Diagonal( ncol(mat), 1 / gfs )
    lpmat <- pmat
    lpmat@x <- log( lpmat@x + 1 )
    globalWeights <- 1 + colSums( pmat * lpmat ) / log( nrow(mat) )
    
  } else {
    stop( "Unknown global weight function specification for the argument globalWeightFunction.", call.=TRUE)
  }
 
  globalWeights 
}

#' Document-term matrix term re-weighting.
#' @description Applies the term weight functions -- like Inverse Document Frequency (IDF) -- 
#' to the entries of a sparse matrix.
#' @param docTermMat A document-term sparse matrix (dgCMatrix).
#' @param globalWeightFunction A global weight function ID or
#' a numeric vector with length that equals \code{ncols(docTermMat)}.
#' @param localWeightFunction A global weight function ID (a string, one of "Binary", "TermFrequency", "Log", "None").
#' @param normalizerFunction A normalization weight function ID (a string, one of "Cosine", "Sum", "None"),
#' @return Sparse matrix
#' @details  The implemented global weight function ID's are "IDF", "GFIDF", "Normal", "Binary", "Entropy", "Sum", "None".
#' The implemented local weight function ID's are "Binary", "TermFrequency", "Log", "Logarithmic", "None".
#' The implemented normalization function ID's are "Cosine", "Sum", "None".
#' @family Document-term matrix functions
#' @export
SMRApplyTermWeightFunctions <- function( docTermMat, globalWeightFunction = NULL, localWeightFunction = NULL, normalizerFunction = NULL ) {

  if ( class(docTermMat) != "dgCMatrix" || nrow(docTermMat) < 1 || ncol(docTermMat) < 1 ) {
    stop( "The argument docTermMat is expected to be a sparse matrix with number of rows and columns greater than zero", call.=TRUE)
  }

  mat <- docTermMat
  
  if ( is.null(globalWeightFunction) ) { globalWeightFunction = "None" }
  if ( is.null(localWeightFunction) ) { localWeightFunction = "None" }
  if ( is.null(normalizerFunction) ) { normalizerFunction = "None" }

  # Global weights set-up.
  if( is.character(globalWeightFunction) ) {
    
    globalWeights <- SMRGlobalTermFunctionWeights( docTermMat = mat, globalWeightFunction = globalWeightFunction )
    
  } else if ( is.numeric(globalWeightFunction) && length(globalWeightFunction) == ncol(docTermMat) ) {
    
    globalWeights <- globalWeightFunction
    
  } else {
    
    stop( "The argument globalWeightFunction is expected to be a string or a numeric vector with length that equals ncols(docTermMat).", call. = TRUE )
    
  }
  
  # Local weights application.
  if( missing(localWeightFunction) || is.null(localWeightFunction) ) {

    if ( ! ( is.null(globalWeightFunction) || missing(globalWeightFunction) ) ) {

      diagMat <- Diagonal(ncol(docTermMat), globalWeights)
      mat <- mat %*% diagMat

    }

  } else {

    if ( localWeightFunction == "TermFrequency" ||  localWeightFunction == "None" ) {
      ## There is nothing to be done if localWeightFunction is "None" or "TermFrequency".

    } else if ( localWeightFunction == "Binary" ) {

      mat@x[ mat@x > 0 ] <- 1

    } else if ( localWeightFunction == "Log" || localWeightFunction == "Logarithmic"  ) {

      mat@x <- log( mat@x + 1 )

    } else {
      stop( "Unknown local weight function specification for the argument localWeightFunction.", call.=TRUE)
    }

    ## Multiply with the global weights
    if ( ! ( is.null(globalWeightFunction) || missing(globalWeightFunction) ) ) {

      diagMat <- Diagonal(ncol(docTermMat), globalWeights)
      mat <- mat %*% diagMat

    }
  }

  # Normalizing.
  if( !( missing(normalizerFunction) || is.null(normalizerFunction) ) ) { ## || normalizerFunc == identity

    if( class(normalizerFunction)[[1]]=="character" ) {
      if ( normalizerFunction == "None" ) {

        ## do nothing

      } else if ( normalizerFunction == "Cosine" ) {

        svec <- sqrt( rowSums( mat * mat ) )
        svec <- ifelse( svec > 0, svec, 1 )
        mat <- mat / svec

      } else if ( normalizerFunction == "Sum" || normalizerFunction == "RowStochastic" ) {

        svec <- rowSums( mat )
        svec <- ifelse( svec > 0, svec, 1 )
        mat <- mat / svec

      } else if ( normalizerFunction == "Max" || normalizerFunction == "Maximum" ) {

        warning( "The normalization per row with the argument normalizerFunction set to 'Max' or 'Maximum' is potentially very slow." )

        svec <- purrr::map_dbl( 1:nrow(mat), function(i) { max( mat[i,,drop=FALSE] ) })
        svec <- ifelse( svec > 0, svec, 1 )
        mat <- mat / svec
        
      } else if ( normalizerFunction == "AbsMax" || normalizerFunction == "AbsMaximum" ) {
        
        warning( "The normalization per row with the argument normalizerFunction set to 'AbsMax' or 'AbsMaximum' is potentially very slow." )
        
        svec <- purrr::map_dbl( 1:nrow(mat), function(i) { max( abs(mat[i,,drop=FALSE]) ) })
        svec <- ifelse( svec > 0, svec, 1 )
        mat <- mat / svec

      } else {
        stop( "Unknown normalizing function specification for the argument normalizerFunction.", call.=TRUE)
      }
    }
  }

  # Result.
  mat
}

#' Make a matrix column stochastic.
#' @description Makes the matrix argument a column stochastic matrix (the sum of each column is 1).
#' @param mat A matrix (sparse or dense).
#' @return Sparse matrix
#' @family Document-term matrix functions
#' @export
SMRMakeColumnStochastic <- function( mat ){
  if ( !( class(mat) == "Matrix" || class(mat) == "dgCMatrix") ) {
    stop("Matrix is expected as an argument", call.=TRUE)
  }
  globalWeights <- colSums(mat)
  globalWeights[ globalWeights == 0 ] <- 1
  globalWeights <- 1 / globalWeights

  diagMat <- Diagonal(ncol(mat), globalWeights)
  mat <- mat %*% diagMat
  mat
}

#' Make a matrix row stochastic.
#' @description Makes the matrix argument a row stochastic matrix (the sum of each row is 1).
#' @param mat A matrix (sparse or dense).
#' @return Sparse matrix
#' @family Document-term matrix functions.
#' @export
SMRMakeRowStochastic <- function( mat ){
  if ( !( class(mat) == "Matrix" || class(mat) == "dgCMatrix") ) {
    stop("Matrix is expected as an argument", call.=TRUE)
  }
  globalWeights <- rowSums(mat)
  globalWeights[ globalWeights == 0 ] <- 1
  globalWeights <- 1 / globalWeights

  diagMat <- Diagonal(nrow(mat), globalWeights)
  mat <- diagMat %*% mat
  mat
}

#' Conversion to bag of words.
#' @description Converts a string into a bag of words.
#' @param text The text to be converted.
#' @param split Character vector containing regular expressions.
#' @param punctuationPattern Pattern for the punctuation signs.
#' @param patternToIgnore Pattern for words to be ignored.
#' @family Document-term matrix functions
#' @export
SMRToBagOfWords <- function( text, split = "\\W", punctuationPattern = "[[:punct:]]",
                             stopWords = NULL, minWordLength = 2, patternToIgnore = NULL ) {

  if ( !is.null(patternToIgnore) ) {
    m <- gregexpr( pattern = patternToIgnore, text )
    res <- unlist( strsplit( unlist( regmatches( text, m, invert = TRUE ) ), split = split ) )
  } else {
    res <- unlist( strsplit( text, split = split ) )
  }

  res <- gsub( pattern = punctuationPattern, replacement = "", res )

  if( !is.null(stopWords) ) {
    res <- res[ !( res %in% stopWords) ]
  }

  if ( minWordLength > 0  ) {
    res[ nchar(res) >= minWordLength ]
  } else { res }
}

#' Create document-term matrix from a document collection.
#' @description Creates a document-term matrix from a list of documents and list of ID's
#' @param documents A list of documents.
#' @param ids A list of ID's corresponding to the documents.
#' @param split A string pattern to split with.
#' @param applyWordStemming Should word stemming be applied or not?
#' @details It would be nice if this function uses SMRToBagOfWords function defined above.
#' @return Sparse matrix
#' @family Document-term matrix functions
#' @export
SMRMakeDocumentTermMatrix <- function( documents, ids = NULL, split = "\\W",
                                       applyWordStemming = TRUE, minWordLength = 2 ) {

  if ( is.null(ids) ) { 
    ids <- paste0( "id", formatC( x = 1:length(documents), width = ceiling(log10(length(documents))) + 1, flag = "0" ) ) 
  }

  if ( length(documents) != length(ids) ) {
    stop( "The lengths of the arguments 'documents' and 'ids' are expected to be the same.", call. = TRUE )
  }

  minWordLength <- max( minWordLength, 1 )
  
  ## Split the descriptions into words
  ss <- setNames( strsplit( documents, split = split ), ids )

  ## Remove words that are too short
  ss <- ss[ purrr::map(ss, length) > 0 ]
  ss <- purrr::map( ss, function(x) x[ nchar(x) >= minWordLength ] )

  ## Convert all words to lower case and apply stemming
  snLoadQ = exists("wordStem")
  if(  applyWordStemming && !snLoadQ ) {
      warning("The function 'wordStem' does not exist. Attempting to proceed by loading the library `SnowballC`.")
      snLoadQ = require("SnowballC")
  }
  if ( applyWordStemming && snLoadQ ) {
    ss <- purrr::map( ss, function(x) wordStem( tolower(x) ) )
  } else {
    ss <- purrr::map( ss, function(x) tolower(x) )
  }
  ss <- ss[ purrr::map(ss, length) > 0 ]

  ## Make document-term contingency matrix
  ssDF <- purrr::map_df( 1:length(ss), 
                         function(i) { 
                           data.frame( id = names(ss)[i], term = ss[[i]], stringsAsFactors = FALSE ) 
                         })
  dtMat <- xtabs( formula = ~ id + term, ssDF, sparse = TRUE )

  dtMat
}
