#=======================================================================================
# Sparse matrix recommender framework extra functions in R
# Copyright (C) 2020  Anton Antonov
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

#' @import Matrix
#' @import dplyr
#' @import purrr
NULL


##===========================================================
## SMRTagTypeRepresentation
##===========================================================

SparseMatrixRecommenderQ <- function(obj) {
  mean( c( "M", "M01", "TagTypeRanges", "TagTypes", "ItemColumnName" ) %in% names(obj) ) == 1
}
 
ListOfSparseMatrixRecommendersQ <- function(obj) {
  
  if( !is.list(obj) ) { 
    FALSE
  } else {
    res <- purrr::map( obj, function(x) { SparseMatrixRecommenderQ(x) })
    mean(res) == 1
  }

} 

#' SMR's tag types representation
#' @description Gives a data frame with the tag types representation statistics for a list of recommenders.
#' @param recommenders A list of recommenders.
#' @return A data frame.
#' @details If the list of recommenders does not have element names automatic ordinal names are assigned.
#' @export
SMRTagTypeRepresentation <- function( recommenders ) {

  if( !ListOfSparseMatrixRecommendersQ( recommenders ) ) {
    stop( "The first argument, recommenders, is expected to be a list of (sparse matrix) recommender objects.", call. = TRUE )
  }
  
  if( is.null( names(recommenders) ) ) {
    names(recommenders) <- seq( 1, length(recommenders) )  
  }
  
  dfFillInQueries <- 
    purrr::map_df( names(recommenders), function(smrName) {
      
      dfRes <- 
        purrr::map_df( recommenders[[smrName]] %>% SMRMonTakeTagTypes, function(tt) {
          
          smat <- SMRSubMatrix( recommenders[[smrName]], tt )
          
          if( !SMRSparseMatrixQ(smat) ) {
            NULL
          } else {
            data.frame( 
              RecommenderName = smrName, 
              TagType = tt, 
              NonZeroCount = sum( rowSums(smat) > 0 ), 
              NonZeroFraction = mean( rowSums(smat) > 0 ), 
              stringsAsFactors = FALSE )
          }
          
        })
      
      dfRes
    })
  
  dfFillInQueries
}


##===========================================================
## SMRSparseMatrixCor: cor for sparse matrices
##===========================================================
## See https://stackoverflow.com/a/5892652 .

#' Correlation for a sparse matrix
#' @description Gives the correlation of the columns of a sparse matrix.
#' @param x A sparse matrix
#' @return A matrix
#' @details Should produces the same results as \code{cor(as.matrix(x))}.
#' (Follows the namings of \link{\code{cor}}.)
#' @export
SMRSparseMatrixCor <- function( x ) {
  
  n <- nrow(x)
  
  cMeans <- colMeans(x)
  cSums <- colSums(x)
  
  # Calculate the population covariance matrix.
  # There's no need to divide by (n-1) as the std. dev is also calculated the same way.
  # The code is optimized to minize use of memory and expensive operations.
  covmat <- tcrossprod( cMeans, (- 2 * cSums + n * cMeans) )
  crossp <- as.matrix( crossprod(x) )
  covmat <- covmat + crossp
  
  # Standard deviations of columns.
  sdvec <- sqrt(diag(covmat))
  
  # Correlation matrix.
  covmat/crossprod(t(sdvec))
}

