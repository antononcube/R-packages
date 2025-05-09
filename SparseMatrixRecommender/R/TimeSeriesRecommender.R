#=======================================================================================
# Time series recommender framework in R
# Copyright (C) 2016-2017  Anton Antonov
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
# ʇǝu˙oǝʇsod@ǝqnɔuouoʇuɐ,
# Windermere, Florida, USA.
#
#=======================================================================================

#=======================================================================================
# This file contains code of a framework for time series search and recommendation.
# The file includes functions for ground analysis and pre-processing that are most likely 
# needed in order to derive effective recommendations.
#
# For the SMR* functions see the file:
# https://github.com/antononcube/MathematicaForPrediction/blob/master/R/SparseMatrixRecommender.R
#
#=======================================================================================

#' @import purrr
#' @import reshape2
#' @import Matrix
#' @import stringr
NULL

#' Time series recommendations finding
#' @description Finds time series recommendations for specified search row ID and / or search vector ID.
#' @param timeSeriesMat A matrix with time series.
#' @param smr An SMR object (e.g. based on time averaged signals).
#' @param itemIDtoNameRules Item ID's to names rules.
#' @param searchRowID Search row ID that is in \code{rownames(timeSeriesMat) == rownames(smr$M)} or NULL.
#' @param searchVector Search vector with dimension \code{ncol(timeSeriesMat) == ncol(smr$M)} or NULL.
#' @param nrecs Number of recommendations to return; if NULL all recommendations are returned.
#' @param smr.nrecs Number of recommendations for the intermediate candidate finding using the SMR object.
#' @param method Correlation method; one of \code{'dot'}, \code{'pearson'}, \code{'spearman'}, \code{'kendall'}.
#' @param filterRowIDs A character vector with row IDs to filter the rows of the time series and recommender matrices.
#' If NULL not filtering is done.
#' @details The rownames of \code{timeSeriesMat} are composed of channels and item IDs. An item ID can be in several
#' different channels. Hence we have row IDs like "NYSE:APPL" or "NASDAQ:APPL".
#' Note, that this is a package internal function and it is not expected to be used on a regular basis;
#' see \code{\link{Recommendations.TSCorrSMR}}.
#' @export
TSPSRCorrelationNNs <- function( timeSeriesMat, smr, itemIDtoNameRules, 
                                 searchRowID = NULL, searchVector = NULL, 
                                 nrecs = 12, smr.nrecs = 2000, 
                                 method = 'pearson',
                                 filterRowIDs = NULL) {
  
  if( !( is.matrix(timeSeriesMat) || SparseMatrixRecommender::SMRSparseMatrixQ(timeSeriesMat) ) ) {
    stop( "The argument timeSeriesMat is expected to be a matrix or a sparse matrix.", call. = TRUE )
  }
  
  if ( is.null(searchRowID) && is.null(searchVector) ) {
    stop( "At least one of the arguments searchRowID or searchVector has to be non-NULL.", call. = TRUE )
  }
  
  ## Search vector for the SMR object
  if ( !is.null( smr ) ) { 
    
    if ( is.numeric(searchVector) ) {
      searchVectorMat <- sparseMatrix( i = rep(1, length(searchVector) ), j = 1:length(searchVector), x = searchVector, dims = c( 1, ncol(smr$M) ) )
    } else {
      if ( is.null(searchRowID) ) {
        stop( "If searchRowID is NULL then searchVector is expected to be numeric.", call. = TRUE )
      }
      searchVectorMat <- NULL
    }
    
    if ( !is.null( searchRowID ) && !is.null( searchVectorMat ) )  {
      searchVectorMat <- smr$M[ searchRowID, , drop = FALSE ] + searchVectorMat
    } else if ( is.null(searchVectorMat) ) {
      searchVectorMat <- smr$M[ searchRowID, , drop = FALSE ]
    }
    
  } 

  if( mean( rownames(smr$M) == rownames(timeSeriesMat) ) < 1 ) {
    stop( "The time series matrix and recommender matrix have different row names.", call. = TRUE)
  }
  
  ## Search vector for the correlation matrices
  if ( !is.null( searchRowID ) && !is.null( searchVector ) )  {
    searchVector <- timeSeriesMat[ searchRowID, ] + searchVector
  } else if ( is.null(searchVector) ) {
    searchVector <- timeSeriesMat[ searchRowID, ]
  }  
  
  ## Filter rows
  if( !is.null(filterRowIDs) ) {
    
    if( !is.character(filterRowIDs) ) {
      stop("The argument filterRowIDs is expected to be a character vector or NULL.", call. = TRUE)
    }
    
    filterRowIDs <- intersect( filterRowIDs, rownames(timeSeriesMat) )
    
    if( length(filterRowIDs) == 0 ) {
      
      warning("None of the elements of the argument filterRowIDs is a known row ID.", call. = TRUE )
      
    } else {

      filterRowIDs <- unique( c(searchRowID, filterRowIDs) )

      timeSeriesMat <- timeSeriesMat[ filterRowIDs, , drop = FALSE]      
      smr$M <- smr$M[ filterRowIDs, , drop = FALSE]
    }
  }
  
  ## NNs computation
  if ( is.null(smr) ) {
    ## Use only the correlations of the TS matrix rows

    recVec <- cor( as.matrix( t(timeSeriesMat) ), searchVector, method = method )
    recVec <- recVec[ order(-recVec[,1]), ,drop=FALSE]
    recsItemSplit <- setNames( as.data.frame( stringr::str_split_fixed( rownames(recVec), pattern = ":", n = 2 ), stringsAsFactors = F), c( "Channel", "ItemID" ) )
    if( mean( nchar( recsItemSplit$ItemID ) ) < 1 ) {
      recsItemSplit <- data.frame( Channel = "None", ItemID = rownames(recVec), stringsAsFactors = FALSE)
    }
    corRecs <- data.frame( Score = recVec, 
                           Channel.ItemID = rownames(recVec),
                           recsItemSplit, 
                           ItemName = itemIDtoNameRules[ recsItemSplit$ItemID ],
                           stringsAsFactors = FALSE, row.names = NULL  )
    if ( is.null(nrecs)) { corRecs } else { corRecs[ 1:nrecs, ] }
    
  } else {
    ## Use the SMR object first, and then correlations between the TS matrix rows.

    recs <- SMRRecommendationsByProfileVector( smr, searchVectorMat, nrecs = smr.nrecs )
    recsItemSplit <- setNames( as.data.frame( stringr::str_split_fixed( recs[[smr$ItemColumnName]], pattern = ":", n = 2 ), stringsAsFactors = F), c( "Channel", "ItemID" ) )
    if( mean( nchar( recsItemSplit$ItemID ) ) < 1 ) {
      recsItemSplit <- data.frame( Channel = "None", ItemID = recs[[smr$ItemColumnName]], stringsAsFactors = FALSE)
    }
    dotRecs <- cbind( Score = recs$Score, 
                      Channel.ItemID = recs[[smr$ItemColumnName]],
                      recsItemSplit, 
                      ItemName = itemIDtoNameRules[ recsItemSplit$ItemID ],
                      stringsAsFactors = FALSE, row.names = NULL ) 
    
    if ( method != 'dot' ) {
      
      ## assertthat::assert_that( mean(dotRecs$Channel.ItemID %in% rownames(timeSeriesMat)) == 1 )
      recVec <- cor( as.matrix( t(timeSeriesMat[ dotRecs$Channel.ItemID, ,drop=FALSE]) ), searchVector, method = method )
      recVec <- recVec[ order(-recVec[,1]), ,drop=FALSE]
      recsItemSplit <- setNames( as.data.frame( stringr::str_split_fixed( rownames(recVec), pattern = ":", n = 2 ), stringsAsFactors = F), c( "Channel", "ItemID" ) )
      if( mean( nchar( recsItemSplit$ItemID ) ) < 1 ) {
        recsItemSplit <- data.frame( Channel = "None", ItemID = rownames(recVec), stringsAsFactors = FALSE)
      }
      corRecs <- data.frame( Score = recVec, 
                             Channel.ItemID = rownames(recVec),
                             recsItemSplit, 
                             ItemName = itemIDtoNameRules[ recsItemSplit$ItemID ],
                             stringsAsFactors = FALSE, row.names = NULL  )      
      if ( is.null(nrecs)) { corRecs } else { corRecs[ 1:nrecs, ] }
      
    } else {
      if ( is.null(nrecs)) { dotRecs } else { dotRecs[ 1:nrecs, ] }
    }
  }
}


#' Time series recommender creation
#' @param timeSeriesMatrix A time series matrix.
#' @param smr A sparse matrix recommender.
#' @param classLabelMatrix A matrix with class labels; each column is for 
#' a different label.
#' @param correlationMethod Correlation method, one of: 
#' 'dot', 'pearson', 'spearman', 'kendall'.
#' @param smrNRecs The number of recommendations for the intermediate candidate 
#' finding using the SMR object \code{smr}.
#' @param itemIDtoNameRules Item ID's to names rules.
#' @param tibNameToTIBRules A vector with named elements that maps time matrix 
#' column names to time interval boundaries.
#' @return A list (an S3 object).
#' @family Creation functions
#' @export
TSCorrSMRCreate <- function( timeSeriesMatrix, smr, 
                             classLabelMatrix = NULL, 
                             correlationMethod = "pearson", 
                             smrNRecs = 2000, 
                             itemIDtoNameRules = NULL,
                             tibNameToTIBRules = NULL ) {
  
  obj <- list( TSMat = timeSeriesMatrix,
               SMR = smr, 
               ClassLabelsMat = classLabelMatrix,
               CorrelationMethod = correlationMethod, 
               SMRNRecs = smrNRecs, 
               ItemIDtoNameRules = 
                 if( !is.null(itemIDtoNameRules) ) { 
                   itemIDtoNameRules 
                 } else {   
                   setNames( rownames(smr$M), rownames(smr$M) ) 
                 },
               TIBNameToTIBRules = 
                 if( !is.null(tibNameToTIBRules) ) {
                   tibNameToTIBRules
                 } else {
                   setNames( 1:ncol(timeSeriesMatrix), colnames(timeSeriesMatrix))
                 } )
  class(obj) <- "TSCorrSMR"
  obj
}

##===========================================================
## Object-Oriented Programming implementations (S3)
##===========================================================
## TSCorrSMR objects are lists with elements
##    list( TSMat = <time series matrix>, SMR = <SMR Object>, ClassLabelsMat = <sparse matrix>, 
##          CorrelationMethod = "pearson", SMRNRecs = 2000, ItemIDtoNameRules = NULL )
## and attribute 
##    class(...) = "TSCorrSMR"
## The dimensions of TSMat and SMR$M have to be the same.

#' Time series recommendations calculation
#' @description Specialization of Recommendations for TSCorrSMR objects.
#' @param x A recommender object.
#' @param historyItems A list of history items.
#' @param historyRatings A list of history ratings.
#' @param nrecs Number of required recommendations.
#' @param removeHistory Should the history be removed or not?
#' @return A data frame with columns "Score", "Item".
#' @export
Recommendations.TSCorrSMR <- function( x, historyItems, historyRatings, nrecs, removeHistory = TRUE, ... ) {

  itemIDtoNameRules = x$ItemIDtoNameRules
  if ( is.null(itemIDtoNameRules) ) { 
    itemIDtoNameRules = setNames( rownames(x$SMR$M01), rownames(x$SMR$M01) ) 
  }  

  if ( length(historyItems) > length(historyRatings) ) { 
    historyRatings <- rep_len( historyRatings, length.out = length(historyItems) )
  }
    
  if ( length(historyItems) != length(historyRatings) ) {
    stop( "The lengths of historyItems and historyRatings are expected to be the same.", call. = TRUE )
  }
  
  if ( mean( historyItems %in% rownames(x$TSMat) ) < 1 ) {
    stop( "Some elements of historyItems are not in the rownames(x$TSMat).", call. = TRUE )
  }
  
  dMat <- sparseMatrix(  i = rep(1,length(historyItems)), j = 1:length(historyItems), x = historyRatings )
  profileVec <- as.numeric( dMat %*% x$TSMat[ historyItems, , drop=F] )
  
  recs <- TSPSRCorrelationNNs( timeSeriesMat = x$TSMat, 
                               smr = x$SMR, 
                               itemIDtoNameRules = itemIDtoNameRules,
                               searchVector = profileVec,
                               nrecs = nrecs + if( removeHistory ) { length(historyItems) } else { 0 },
                               smr.nrecs = x$SMRNRecs, 
                               method = x$CorrelationMethod )
  names(recs) <- gsub( "ItemName", "Item", names(recs) )

  recs <- recs[ , c("Score", "Item") ]
  if ( removeHistory ) {
    recs[ !( recs$Item %in% historyItems ), ]
  }
  
  if( nrow(recs) > nrecs ) {
    recs <- recs[1:nrecs, ]
  }
  
  recs
}


##===========================================================
## Classify
##===========================================================

#' Classification with time series recommender
#' @description Specialization of \code{ClassifyByProfileVector} for time series recommender objects.
#' @param x A time series recommender object ("TSCorrSMR").
#' @param profileVec A profile vector.
#' @param nTopNNs Number of top nearest neighbors to be used.
#' @param voting Should voting method to be used?
#' @details The two methods of making the classifications from the obtained recommendations 
#' are: "voting" and "scoring".
#' @return A data frame with columns "Score", "Label".
#' @export 
ClassifyByProfileVector.TSCorrSMR <- function ( x, profileVec, nTopNNs, voting = FALSE ) {
  
  itemIDtoNameRules = x$ItemIDtoNameRules
  if ( is.null(itemIDtoNameRules) ) { 
    itemIDtoNameRules = setNames( rownames(x$SMR$M01), rownames(x$SMR$M01) ) 
  }  
    
  # if ( !( tagType %in% x$SMR$TagTypes ) ) {
  #   stop( "The argument tagType is expected to be one of the tag types of x$SMR .", call. = TRUE )
  # }
  
  recs <- TSPSRCorrelationNNs( timeSeriesMat = x$TSMat, 
                               smr = x$SMR, 
                               itemIDtoNameRules = itemIDtoNameRules,
                               searchVector = profileVec,
                               nrecs = nTopNNs,
                               smr.nrecs = x$SMRNRecs, 
                               method = x$CorrelationMethod )
  
  names(recs) <- gsub( "ItemName", "Item", names(recs) )
  
  ## This code is copied from SMRClassifyByProfileVector -- 
  ##  read the comments in that code.
  clMat <- x$ClassLabelsMat
  
  if ( voting ) {
    clMat@x[ clMat@x > 0 ] <- 1
    recs$Score <- 1
  }
  s <- (recs$Score / max(recs$Score) ) %*% clMat[ recs$Item, , drop=F]
  s <- data.frame( Score = s[1,], Label = colnames(s), stringsAsFactors = FALSE )
  s[ order(-s[,1]), ] 
}


##===========================================================
## Time series search vectors
##===========================================================

#' Search time series creation
#' @description Creates a list of search vectors (time series) for a given time series matrix.
#' @param tsMat A sparse matrix with rows corresponding to time series.
#' @param nTrigPeriod 
#' @details This "helper" function can be used for finding trends of the signals.
#' @export
MakeTimeSeriesSearchVectors <- function( tsMat ) {
  
  ## Search for trends
  tsSearchVectors <- c()
  
  ## Straight ascending line
  searchVector <- ( 1:ncol(tsMat) ) / ncol(tsMat)
  tsSearchVectors <- c( tsSearchVectors, list(StraightUp = searchVector) )
  
  ## Straight descending line
  searchVector <- seq( ncol(tsMat), 1, -1 ) / ncol(tsMat)
  tsSearchVectors <- c( tsSearchVectors, list(StraightDown = searchVector) )
  
  ## Increasing in the last half
  searchVector <- ( 1:ncol(tsMat) ) - ( ncol(tsMat) / 2 ); searchVector[ searchVector < 0 ] <- 0
  searchVector <- searchVector / ncol(tsMat)
  tsSearchVectors <- c( tsSearchVectors, list(SecondHalfUp = searchVector) )
  
  ## Decreasing in first half, increasing in the last half
  searchVector1 <- ( 1:ncol(tsMat) ) - ( ncol(tsMat) / 2 ); searchVector1[ searchVector1 < 0 ] <- 0
  searchVector2 <- rev( 1:ncol(tsMat) ) - ( ncol(tsMat) / 2 ); searchVector2[ searchVector2 < 0 ] <- 0
  searchVector <- searchVector1 + searchVector2
  searchVector <- searchVector / ncol(tsMat)
  tsSearchVectors <- c( tsSearchVectors, list(DownAndUp = searchVector) )
  
  ## Decreasing in first half, increasing in the last half
  tsSearchVectors <- c( tsSearchVectors, list(UpAndDown = (-searchVector) + max(searchVector) ) )
  
  ## Sin
  searchVector <- sin( 1:ncol(tsMat) / ( 1.5 * 10 ) )
  searchVector <- searchVector + 1
  tsSearchVectors <- c( tsSearchVectors, list(Sin = searchVector) )
  
  
  ## Cos
  searchVector <- 1-cos( 1:ncol(tsMat) / ( 1.5 * 10 ) )
  searchVector <- searchVector + 1
  tsSearchVectors <- c( tsSearchVectors, list(Cos = searchVector) )

  tsSearchVectors
}
