#=======================================================================================
# Sparse matrix recommender framework in R
# Copyright (C) 2014-2016  Anton Antonov
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

#=======================================================================================
# Initially this code was made to resemble the Sparse Matrix Recommender Mathematica
# package [1] as closely as possible, but an approach more inherent to R was taken.
# Namely, the columns and the rows of the metadata matrix are named, and because of this
# tag-index and item-index rules are not required.

# The tag-index and item-index rules are made with integer arrays with named entries.

# I did consider programming and using a S4 object, but that requires the declaration of
# too many generic functions. And because inheritance is not essential I kept the object
# in a list.

# There should be separate files (packages) for term weights and outlier detection.
# See the notes below.

# [1] Anton Antonov, Sparse matrix recommender framework in Mathematica,
#     SparseMatrixRecommenderFramework.m at MathematicaForPrediction project at GitHub, (2014).
#     URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/SparseMatrixRecommenderFramework.m
#
# History
# Started: November 2013,
# Updated: December 2013, May 2014, June 2014, July 2014, December 2014,
# January 2016, June 2016, September 2016.
# Converted into to an R package January 2019.
#=======================================================================================
#
# TODO Argument type and ranks check
# Mathematica has pattern matching for the arguments, here I have to make type checks.
# Note that S4 provides some of this functionality.
#
# TODO Union of two SMRs by sub-matrices
# [ ] Given two matrices of the same tag type data for two SMRs make
#     one matrix that have the union of the rownames and colnames.
#     - Take care of collisions.
#       - Trivially done with matrix summation and clipping.
# [ ] Make the SMR sub-matrix union for
#     - a given pair of SMRs, and
#     - a given a list of pairs of tag types.
#---------------------------------------------------------------------------------------

# 05/02/14
# I am not sure:
# 1. should the recommendation request functions take data frames,
# 2. should the scores be the first column (as in the Mathematica code).
# These points need more design effort.

# 05/12/14
# After a conversation with a coworker: it is better instead of an array for tag type
# offsets to use a data frame with the column ranges of the tag types.

# 07/30/14
# 1. Refactored the code for creation of SMR objects: two signatures from transactions,
# and from matrices.
# 2. Extracted the document-term weight functions in a separate file:
# DocumentTermWeightFunctions.R
#
# 12/23/14
# Added the function SMRReorderRecommendations that re-orders recommendations according
# to scores from common tags.
#
# 2016-01-05
# Introduced of S3 OOP (i.e. generic function) implementations for the recommender objects.
# A recommender object is just a list with named elements. It is turned into an
# S3 object by assigning "SMR" to the attribute class.
# This file has only set-up and implementations for SMR's. Other type of recommenders
# have to provided the corresponding generic functions.
# At this point they are three:
# 1. Recommendations (e.g. Recommendations.SMR )
# 2. RecommenderTags (e.g. RecommenderTags.SMR )
# 3. RecommenderItems (e.g. RecommenderItems.SMR )
#
# 2016-06-05
# Added an implementation of the Composite pattern for combined recommendations.
# - Application of the Composite Design Pattern for a collection of recommenders using S3 objects
#   For example, SMR, PageRank recommenders, and HubItemDynamicRanks recommender.
# - Implemented a function for the combination of recommendations from many recommenders.
# - There is an argument allowing the merging of the recommendations to be done
#   according to different types normalizations.
#
# 2016-06-08
# Added functions for converting the SMR sparse matrices data into data frames
# both (long and wide forms).
#
# 2016-09-12
# Added a classification computation function for a profile vector based on
# specified number of top NNs.
#=======================================================================================

#' @import dplyr
#' @import Matrix
#' @import purrr
#' @import reshape2
NULL

#' Creation of an item-tag contingency matrix
#' @description Convert to contingency matrix from item consumption "transactions" (e.g. instances of movie watching).
#' @param dataRows A data frame corresponding to a item consumption metadata table.
#' @param itemColumnName A name of the column of \code{dataRows} the values of which correspond to the rows of the returned matrix.
#' @param tagType A name of the column of \code{dataRows} the values of which correspond to the columns of the returned matrix.
#' @param sparse A logical, should the returned matrix be sparse or not.
#' @return A sparse matrix or a matrix.
#' @family Creation functions
#' @export
SMRCreateItemTagMatrix <- function( dataRows, itemColumnName, tagType, sparse = TRUE ) {
  
  if( mean(is.na(dataRows[[tagType]])) == 1 ) {
    res <- sparseMatrix( i = c(1), j = c(1), x = c(0), dims = c( nrow(dataRows), 1 ) )
    rownames(res) <- dataRows[[itemColumnName]]
    colnames(res) <- "NA"
    return(res)
  }
  
  formulaString <- paste("~", itemColumnName, "+", tagType)
  xtabs( formula = as.formula(formulaString), data = dataRows, sparse = sparse )
}

#' Creation of a SMR object from a transactions data frame
#' @description Creates a Sparse Matrix Recommender object from transactions data and a list of tag types.
#' @param dataRows The transactions data frame.
#' @param tagTypes The name of the column containing the categorical tags.
#' @param itemColumnName The name of the column containing the unique items.
#' @details An S3 object is returned that is list with class attribute set to "SMR".
#' @return SMR object.
#' @family Creation functions
#' @export
SMRCreate <- function(dataRows, tagTypes = names(dataRows)[-1], itemColumnName = names(dataRows)[1] ){
  
  matrices <- purrr::map(tagTypes, function(x){
    SMRCreateItemTagMatrix(dataRows, tagType=x, itemColumnName=itemColumnName)
  })
  
  allRowNames <- sort(unique(unlist(purrr::map( matrices, rownames ))))
  matrices <- purrr::map( matrices, function(x) SMRImposeRowIDs( rowIDs = allRowNames, smat = x ) )                    
    
  SMRCreateFromMatrices(matrices, tagTypes, itemColumnName)
}

#' Creation of a SMR object with a list of matrices
#' @description Creates a Sparse Matrix Recommender object from a list of matrices and a corresponding list of tag types.
#' @param matrices A list of matrices to be spliced into a metadata matrix.
#' @param tagTypes Vector of matrix names.
#' @param itemColumnName The column name of recommender items (in data and recommendations).
#' @details An S3 object is returned that is list with class attribute set to "SMR".
#' @return SMR object.
#' @family Creation functions
#' @export
SMRCreateFromMatrices <- function( matrices, tagTypes = names(matrices), itemColumnName = "Item" ){
  
  if( is.null(tagTypes) ) { 
    tagTypes <- names(matrices)
    if( length(unique(names(matrices))) < length(matrices) ) {
      stop( "When tagTypes = NULL the elements of the argument matrices are expected to have unique names.", call. = TRUE )
    }
  }
  
  if ( length(matrices) != length(tagTypes)  ) {
    stop("The same number of matrices and tag types is required.", call.=TRUE)
  }
  
  m <- do.call(cbind, matrices)

  widths <- purrr::map_int(matrices, function(x){ncol(x)})
  ends <- cumsum(widths)
  begins <- ends - widths + 1
  ranges <- data.frame(Begin=begins, End=ends)
  rownames(ranges) <- tagTypes
  
  tagToIndexRules <- 1:ncol(m)
  names(tagToIndexRules) <- colnames(m)
  
  itemToIndexRules <- 1:nrow(m)
  names(itemToIndexRules) <- rownames(m)
  
  res <- list( M=m, M01=m, TagTypeRanges=ranges, TagTypes=tagTypes, ItemColumnName=itemColumnName,
               TagToIndexRules=tagToIndexRules, ItemToIndexRules=itemToIndexRules )
  class(res) <- "SMR"
  res
}

#' Empty specification
#' @description Creates an empty specification data frame for the creation of 
#' a sparse matrix recommender.
#' @param nrow Number of rows.
#' @return A data frame.
#' @details See \code{\link{SMRCreateFromSpecification}}.
#' @family Creation functions
#' @export
SMREmptySpecification <- function( nrow = 1 ) {
  
  data.frame( "ColumnName" = rep(NA, nrow),
              "ValueColumnName" = rep("None", nrow),
              "GlobalWeightFunction" = rep("None", nrow), 
              "LocalWeightFunction" = rep("None", nrow),
              "NormalizerFunction" = rep("None", nrow),
              "NormalizeByMax" = rep(FALSE, nrow),
              stringsAsFactors = FALSE)
  
}

#' Creation of a Sparse Matrix Recommender object from a specification
#' @description Creates a sparse matrix recommender from transactions-like data
#' and a meta-data specification.
#' @param data A transactions-like data frame.
#' @param metaDataSpec A data frame with specifications of which columns of
#' \code{data} to be used and with what weight functions.
#' @param itemColumnName The name of the column containing the unique items.
#' @details The specification data frame is expected to have the columns names:
#' "ColumnName", "ValueColumnName", "GlobalWeightFunction", "LocalWeightFunction", "NormalizerFunction", "NormalizeByMax".
#' The NA values of "ValueColumnName" are replaced with "None". 
#' (I.e. do not use "None" as a "value" column name of \code{data}.)
#' See \code{\link{SMREmptySpecification}}.
#' @return An SMR object.
#' @family Creation functions
#' @export
SMRCreateFromSpecification <- function( data, metaDataSpec, itemColumnName ) {
  
  if( class(data) != "data.frame" || class(metaDataSpec) != "data.frame" ) {
    stop("The first and second arguments are expected to be data frames.")
  }
  
  matrices <- purrr::map(as.character(metaDataSpec$ColumnName), 1, function(x){
    SMRCreateItemTagMatrix( dataRows = data, tagType = x, itemColumnName = itemColumnName, sparse = T)
  })
  
  
  if( !( "ValueColumnName" %in% colnames(metaDataSpec) ) ) {
    metaDataSpec <- cbind( metaDataSpec, ValueColumnName = NA )
  }
  
  metaDataSpec$ValueColumnName[ is.na(metaDataSpec$ValueColumnName) ] <- "None"
  
  matrices <-
    purrr::map( split( metaDataSpec, metaDataSpec[, c("ColumnName", "ValueColumnName")]), function(x) {
      
      if ( is.null(x$ValueColumnName) || is.na(x$ValueColumnName) || x$ValueColumnName %in% c("NULL", "None") ) {
        smat <- SMRCreateItemTagMatrix( dataRows = data, tagType = x$ColumnName[[1]], itemColumnName = itemColumnName, sparse = TRUE )
      } else {
        smat <- xtabs( as.formula( paste( x$ValueColumnName[[1]], "~", itemColumnName, "+", x$ColumnName[[1]] ) ), data = data, sparse = TRUE )
      }
      
      smat <- SMRApplyTermWeightFunctions( smat,
                                           x$GlobalWeightFunction[[1]],
                                           x$LocalWeightFunction[[1]],
                                           x$NormalizerFunction[[1]] )
      
      if ( is.logical(x$NormalizeByMax[[1]]) && metaDataSpec$NormalizeByMax[[1]] ) {
        smMax <- max( smat, na.rm = T )
        if( smMax > 0 ) { 
          smat <- smat / smMax
        }
      }
      
      smat
    })

  names(matrices) <- gsub( "\\.NA$", "", names(matrices) )
  names(matrices) <- gsub( "\\.None$", "", names(matrices) )
  
  allRowIDs <- unique( unlist( purrr::map(matrices, function(x) rownames(x) ) ) )
  
  nms <- names(matrices)
  matrices <- purrr::map( matrices, function(x) SMRImposeRowIDs( rowIDs = allRowIDs, smat = x) )
  names(matrices) <- nms
  
  SMRCreateFromMatrices( matrices = matrices, tagTypes = names(matrices), itemColumnName = itemColumnName )
}

#' Apply tag weights
#' @description Changes the weights of the tags of a sparse matrix recommender object
#' @param smr A sparse matrix recommender object (list with named elements).
#' @param weights A list of weights to be applied.
#' @return Sparse matrix.
#' @details Currently there is some mixture (confusion) between "significance factors"
#' of tag types or tags and "weights" of matrix entries.
#' @export
SMRApplyTagWeights <- function( smr, weights ) {
  if ( length(weights) < ncol(smr$M01) ) {
    weights <- rep( weights, ncol(smr$M01) )
  } else if ( length(weights) > ncol(smr$M01) ) {
    weights <- weights[1:ncol(smr$M01)]
  }
  W <- Diagonal(x=weights)
  smr$M01 %*% W
}

#' Normalization by maximum
#' @description Makes all sub-matrices to have elements between 0 and 1.
#' @param smr A sparse matrix recommender object (list with named elements).
#' @return Sparse matrix.
#' @export
SMRNormalizeSubMatricesByMaxEntry <- function( smr ) {
  mWeights <- purrr::map_dbl( smr$TagTypes, function(tt) max( SMRSubMatrix(smr, tt) ) )
  mWeights[ mWeights == 0 ] <- 1
  SMRApplyTagTypeWeights( smr, 1 / mWeights )
}

#' Change tag type weights
#' @description Changes the weights of tag types of a sparse matrix recommender object.
#' @param smr A sparse matrix recommender object (list with named elements).
#' @param weights A list/vector of weights to be applied.
#' @details 
#' If \code{weights} does not have names it is going to replicated to match 
#' the length of \code{smr$TagTypes}.
#' If \code{weights} has names the missing tag types are (if any) 
#' are set to have weight 1.
#' Note that the result of this function is a sparse matrix not an SMR object.
#' (The function name probably have to be changed for consistency: 
#' tag types should have "significance factors" not "weights".)
#' @return Sparse Matrix.
#' @export
SMRApplyTagTypeWeights <- function( smr, weights ) {
  
  if( is.null(names(weights)) ) {
    
    if ( length(weights) < length(smr$TagTypes) ) {
      weights <- rep_len(weights, length.out = length(smr$TagTypes) )
    } else if ( length(weights) > length(smr$TagTypes) ) {
      weights <- weights[1:length(smr$TagTypes)]
    }
    
    weights <- setNames( weights, smr$TagTypes )
    
  } else {

    if( length(weights) == 0 || mean(names(weights) %in% smr$TagTypes) < 1 ) {
      stop( "The names of the weights are expected to be known tag types.", call. = TRUE )
    }
    
    tw <- setNames( rep(1,length(smr$TagTypes)), smr$TagTypes )
    tw[names(weights)] <- weights
    weights <- tw
  }
  
  weights <- weights[smr$TagTypes]
  names(weights) <- NULL
  
  #wvec <- unlist(mlply(cbind(smr$TagTypeRanges,W=weights), function(Begin,End,W) rep(W,End-Begin+1)))
  wvec <- purrr::map( 1:length(smr$TagTypes), function(i) rep( weights[i], smr$TagTypeRanges[i,]$End - smr$TagTypeRanges[i,]$Begin + 1 ) )
  wvec <- do.call(c, wvec)
  SMRApplyTagWeights( smr, wvec )
}


#' Sub-matrix corresponding to a tag type
#' @description Returns the sub-matrix of the SMR metadata matrix that corresponds to a tag type.
#' @param smr A sparse matrix recommender object (list with named elements).
#' @param tagType A tag type.
#' @return Sparse matrix.
#' @export
SMRSubMatrix <- function(smr, tagType ){
  smr$M[,smr$TagTypeRanges[tagType, "Begin"]:smr$TagTypeRanges[tagType, "End"], drop = FALSE ]
}

#' Sub-matrix corresponding to a tag type
#' @description Returns the sub-matrix of a matrix that corresponds to a tag type in an SMR object.
#' @param M A sparse matrix (in a sparse matrix recommender object).
#' @param ranges Column ranges of sub-matrices (in a sparse matrix recommender object).
#' @param tagType A tag type.
#' @return Sparse matrix.
#' @export
SMRSubMatrixOfMatrix <- function( M, ranges, tagType ) {
  M[,ranges[tagType, "Begin"]:ranges[tagType, "End"]]
}

#' Get current tag type significance factors
#' @description Finds the current significance factors in a SMR object.
#' @param smr A sparse matrix object.
#' @return A list of named significance factors (numbers).
#' @export
SMRCurrentTagTypeSignificanceFactors <- function(smr) {
  sfs01 <- purrr::map_dbl( smr$TagTypes, function(tc) sum( SMRSubMatrixOfMatrix( smr$M01, smr$TagTypeRanges, tc ) ) )
  sfs01[ sfs01 == 0 ] <- 1
  res <- purrr::map_dbl( smr$TagTypes, function(tc) sum( SMRSubMatrix( smr, tc ) ) ) / sfs01
  setNames( res, smr$TagTypes )
}


#' Convert a recommendations vector to a data frame
#' @description Convert a recommendations vector to a data frame.
#' @param rvec Recommendations vector.
#' @param history History of items.
#' @param nrecs Number of recommendations to be returned.
#' @param removeHistory Should the history be dropped or not?
#' @return Data frame.
#' @export
SMRRecommendationsVectorToDF <- function( rvec, history, nrecs, removeHistory ) {
  rvec <- as.numeric(rvec)
  if ( is.null(nrecs) ) {
    ## take all non-zero
    recInds <- rev(order(rvec))
    recInds <- recInds[ rvec[recInds] > 0 ]
    nrecs <- length(recInds)
  } else {
    recInds <- rev(order(rvec))[1:(nrecs + length(history))]
  }
  
  if ( removeHistory ) {
    dropInds <- recInds %in% history
    recInds <- recInds[ ! dropInds ]
  }
  
  if ( nrecs < length(recInds) ) {
    recInds <- recInds[1:nrecs]
  }
  recScores <- rvec[ recInds ]
  
  data.frame( Score = recScores, Index = recInds, stringsAsFactors=FALSE )
}

#' Compute recommendations
#' @description Recommend items based on a sparse matrix and user history of consumption.
#' @param smr A sparse matrix recommender.
#' @param userHistoryItems The items the user has consumed / purchased.
#' @param userRatings Ratings of the history items.
#' @param nrecs Number of recommendations to be returned.
#' @param removeHistory Should the history be removed from the recommendations?
#' @return A data frame with columns "Score", "Index", \code{smr$ItemColumnName}.
#' @family Recommendations computation functions
#' @export
SMRRecommendations <- function( smr, userHistoryItems, userRatings, nrecs, removeHistory=TRUE ) {
  
  if ( class(userHistoryItems) != "integer" && class(userHistoryItems) != "numeric" ) {
    userHistoryItems <- match( userHistoryItems, rownames(smr$M) )
  }
  if ( class(userHistoryItems) != "integer" && class(userHistoryItems) != "numeric" ) {
    stop("Row ID's (names or indices) are expected for the argument userHistoryItems.", call.=TRUE)
  }
  if ( class(userRatings) != "numeric" && class(userRatings) != "integer") {
    stop("Positive real numbers are expected for the argument userRatings.", call.=TRUE)
  }
  if ( length(userRatings) < length(userHistoryItems) ) {
    userRatings <- rep( userRatings, length(userHistoryItems) )
  }
  if ( length(userRatings) > length(userHistoryItems) ) {
    userRatings <- userRatings[1:length(userHistoryItems)]
  }
  
  hvec <- sparseMatrix(i=rep(1,length(userHistoryItems)), j=userHistoryItems, x=userRatings, dims=c(1,dim(smr$M)[1]))
  rvec <- smr$M %*% t(hvec %*% smr$M)
  rvec <- as.array(rvec)
  recInds <- rev(order(rvec))[1:(nrecs+length(userHistoryItems))]
  
  if ( removeHistory ) {
    dropInds <- recInds %in% userHistoryItems
    recInds <- recInds[ ! dropInds ]
  }
  
  if ( nrecs < length(recInds) ) {
    recInds <- recInds[1:nrecs]
  }
  recScores <- rvec[ recInds ]
  
  res<-as.data.frame(cbind(recScores,recInds), stringsAsFactors=FALSE)
  res<-cbind(res,rownames(smr$M)[recInds], stringsAsFactors=FALSE)
  names(res)<-c("Score","Index",smr$ItemColumnName)
  res
}

#' Compute recommendations using a history data frame
#' @description Recommend items based on a sparse matrix recommender and a data frame user history of consumption.
#' @param smr A sparse matrix recommender.
#' @param history A data frame of rated items with columns "Ratings", <some-item-ID>.
#' @param nrecs Number of recommendations to be returned.
#' @param removeHistory Should the history be removed from the recommendations?
#' @return A data frame with columns "Score", "Index", \code{smr$ItemColumnName}.
#' @family Recommendations computation functions
#' @export
SMRRecommendationsDF <- function( smr, history, nrecs, removeHistory=TRUE ) {
  if ( is.numeric(history[,2]) ) {
    res <- SMRRecommendations( smr, history[,2], history[,1], nrecs )
  } else {
    inds <- match(  history[,2], rownames( smr$M ) )
    if (  NA %in% inds ) {
      stop("Some of the items are not in the sparse matrix recommender object.")
    }
    res <- SMRRecommendations( smr, inds, history[,1], nrecs, removeHistory )
  }
  names(res) <- c( names(res)[1:2], names(history)[[2]] )
  res
}

#' Recommendations using a profile data frame
#' @description Recommend items based on a sparse matrix and a specified profile.
#' @param smr A sparse matrix recommender.
#' @param profile A data frame of scored tags, profile of a user with column names c( "Score", "Tag" | "Index" )/
#' @param nrecs Number of recommendations to be returned,
#' @return A data frame with columns \code{ c("Score", "Index", smr$ItemColumnName)}.
#' @family Recommendations computation functions
#' @export
SMRRecommendationsByProfileDF <- function( smr, profile, nrecs ) {
  if ( names(profile) == c( "Tag", "Score" ) || names(profile) == c( "Index", "Score" ) ) {
    profile <- profile[,c(2,1)]
  }
  if ( is.numeric( profile[,2] ) ) {
    res <- SMRRecommendationsByProfile( smr, profile[,2], profile[,1], nrecs )
  } else {
    inds <- match(  profile[,2], colnames( smr$M ) )
    if (  NA %in% inds ) {
      stop("Some of the tags are not in the sparse matrix recommender object.")
    }
    res <- SMRRecommendationsByProfile( smr, inds, profile[,1], nrecs )
  }
  res
}

#' Recommendations using two vectors comprising a profile
#' @description Recommend items based on a sparse matrix and a specified profile indices and scores.
#' @param smr A sparse matrix recommender.
#' @param profileInds A vector of metadata indices corresponding to the columns of \code{smr$M}.
#' @param profileRatings A vector of ratings of the profile metadata.
#' @param nrecs Number of recommendations to be returned.
#' @return A data frame with columns \code{ c("Score", "Index", smr$ItemColumnName)}.
#' @family Recommendations computation functions
#' @export
SMRRecommendationsByProfile <- function( smr, profileInds, profileRatings, nrecs ) {
  pvec <- sparseMatrix(i=rep(1,length(profileInds)), j=profileInds, x=profileRatings, dims=c(1,dim(smr$M)[2]))
  SMRRecommendationsByProfileVector( smr, pvec, nrecs )
}

#' Recommendations by profile vector
#' @description Recommend items based on a sparse matrix and specified profile.
#' @param smr A sparse matrix recommender.
#' @param profileVec A sparse matrix with 1 row (a row from a sparse matrix).
#' @param nrecs Number of recommendations to be returned.
#' @return A data frame with columns \code{ c("Score", "Index", smr$ItemColumnName)}.
#' @family Recommendations computation functions
#' @export
SMRRecommendationsByProfileVector <- function( smr, profileVec, nrecs ) {
  if ( dim( profileVec )[[2]] == dim( smr$M )[[2]] ) {
    profileVec <- t(profileVec)
  }
  rvec <- smr$M %*% profileVec
  rvec <- as.array(rvec)
  recInds <- rev(order(rvec))
  recScores <- rvec[recInds]
  if ( nrecs > length(rvec) ) {
    nrecs <- length(rvec)
  }
  res <- data.frame( Score = recScores[1:nrecs], Index = recInds[1:nrecs], stringsAsFactors = FALSE )
  res <- cbind( res, Item = rownames(smr$M)[recInds[1:nrecs]], stringsAsFactors = FALSE )
  names(res)<-c( "Score", "Index", smr$ItemColumnName )
  res
}

#' Classification with a profile vector
#' @description Classify a profile vector into the column names of a tag type sub-matrix.
#' @param smr A sparse matrix recommender.
#' @param tagType Tag type for which the classification is done.
#' @param profileVec A sparse matrix with 1 row (a row from a sparse matrix).
#' @param nTopNNs Number of top nearest neighbors to be used in the derive the classification.
#' @param voting Should simple voting be used or a weighted sum?
#' @param maxNumberOfLabels The maximum number of labels to be returned; 
#' if NULL all found labels are returned.
#' @param normalizeQ Should the scores be normalized?
#' (By dividing by the maximum score.)
#' @details If \code{dropZeroScoredLabels = TRUE} and the recommendations vector 
#' has all zero scores then the return value is \code{NA}.
#' @return A list of scored tags.
#' @export
SMRClassifyByProfileVector <- function( smr, tagType, profileVec, nTopNNs, 
                                        voting = FALSE, dropZeroScoredLabels = TRUE, maxNumberOfLabels = NULL, normalizeQ = TRUE ) {
  
  if( !( tagType %in% smr$TagTypes ) ) {
    stop( "Unknown tag type.", call. = T )
  }
    
  recs <- SMRRecommendationsByProfileVector( smr = smr, profileVec = profileVec, nrecs = nTopNNs )
  
  ## Assuming the class labels sub-matrix is relatively small we can do this:
  ## clMat <- SMRSubMatrix( smr = smr, tagType = tagType )
  ## It can be optimized  using a class label matrix member inside the SMR object.
  ## Hopefully, this is quick enough in most cases:
  clMat <- smr$M[ recs[[smr$ItemColumnName]], smr$TagTypeRanges[tagType, "Begin"] : smr$TagTypeRanges[tagType, "End"], drop=F ]
  
  if ( voting ) {
    clMat@x[ clMat@x > 0 ] <- 1
    recs$Score <- 1
  }
  s <- (recs$Score / max(recs$Score, na.rm = T) ) %*% clMat[ recs[[smr$ItemColumnName]], , drop=F]
  s <- data.frame( Score = s[1,], Label = colnames(s), stringsAsFactors = FALSE )
  s <- s[ order(-s[,1]), ]

  if( dropZeroScoredLabels ) { 
    s <- s[ s$Score > 0, ] 
  }
  
  if( length(s) == 0 ) { 
    return(NA) 
  }
  
  if( is.numeric(maxNumberOfLabels) && maxNumberOfLabels > 0 ) {
    s <- s[ 1:min( maxNumberOfLabels, nrow(s) ), ]
  }
  
  if( normalizeQ && max(s$Score, na.rm = T) > 0 ) {
    s$Score <- s$Score / max(s$Score, na.rm = T)
  }
  
  s
}

#' Profile vector calculation
#' @description Calculate profile vector from item history.
#' @param smr A sparse matrix recommender.
#' @param itemHistory A data frame with items history with column names \code{ c("Rating", smr$ItemColumnName)}.
#' @return Sparse matrix.
#' @export
SMRProfileVector <- function( smr, itemHistory ) {
  pinds <- match( itemHistory[,2], rownames(smr$M) )
  names(itemHistory) <- c("Rating", "Item")
  hvec <- sparseMatrix( i=rep(1,nrow(itemHistory)), j=pinds, x=itemHistory$Rating, dims=c(1,dim(smr$M)[1]) )
  pvec <- hvec %*% smr$M
  t(pvec)
}

#' Profile vector calculation
#' @description Calculate profile from item history.
#' @param smr A sparse matrix recommender.
#' @param itemHistory A data frame with item history with column names c("Rating", "Item").
#' @return A data frame with columns "Score", "Index", "Tag".
#' @export
SMRProfileDF <- function( smr, itemHistory ) {
  if( sum( colnames(itemHistory) %in% c("Rating", "Item") ) == 2 ) {
    itemHistory <- itemHistory[, c("Rating", "Item")]
  } else if ( sum( colnames(itemHistory) %in% c("Rating", smr$ItemColumnName ) ) == 2) {
    itemHistory <- itemHistory[, c("Rating", smr$ItemColumnName )]
  }
  pvec <- SMRProfileVector( smr, itemHistory )
  pvecInds <- which( pvec > 0 )
  pvecScores <- pvec[ pvecInds ]
  res <- data.frame( Score = pvecScores, Index = pvecInds, stringsAsFactors = FALSE  )
  res <- cbind( res, Tag = colnames(smr$M)[ pvecInds ], stringsAsFactors = FALSE )
  names(res) <- c("Score","Index","Tag")
  res[ rev( order(res$Score) ),]
}

#' Conversion of a profile vector to a data frame
#' @description Convert a data frame corresponding to a profile vector.
#' @param smr A sparse matrix recommendation object.
#' @param pvec A sparse matrix with one column.
#' @return A data frame with columns "Score", "Index", "Tag".
#' @export
SMRProfileDFFromVector <- function( smr, pvec ) {
  
  if( !( ncol(pvec) == 1 && nrow(pvec) == ncol(smr$M) || nrow(pvec) == 1 && ncol(pvec) == ncol(smr$M) ) ) {
    warning( "It is expected the number of columns/rows of the profile vector to be 1 and its number of rows/columns to be the same as the number of columns of the SMR matrix.",
             call. = T)
  }
  
  pvecInds <- which( pvec > 0 )
  pvecScores <- pvec[ pvecInds ]
  res <- data.frame( Score = pvecScores, Index = pvecInds, stringsAsFactors = FALSE )
  res <- cbind( res, Tag = colnames(smr$M)[ pvecInds ], stringsAsFactors = FALSE )
  names(res) <- c("Score","Index","Tag")
  res[ rev( order(res$Score) ), ]
}

#' Conversion of a profile data frame to a vector
#' @description Return a vector corresponding to a profile data frame.
#' @param smr A sparse matrix recommender.
#' @param profile A data frame with names "Score", "Index", "Tag".
#' @param tagType Tag type over which the vector is made.
#' @param uniqueColumns Should the tags in the profile have unique indices in the columns of \code{smr$M}?
#' @return A sparse matrix with one column.
#' @export
SMRProfileDFToVector <- function( smr, profileDF, tagType = NULL, uniqueColumns = TRUE ) {
  if ( length( intersect( names(profileDF), c("Score", "Index" ) ) ) == 2 ) {
    sparseMatrix( i = profileDF$Index, j = rep(1,nrow(profileDF)), x = profileDF$Score, dims = c( ncol(smr$M), 1 ) )
  } else if ( length( intersect( names(profileDF), c("Score", "Tag" ) ) ) == 2  ) {
    if ( is.null(tagType) ) {
      inds <- which( colnames( smr$M ) %in% profileDF$Tag )
      if ( uniqueColumns ) {
        if (length(inds) != nrow(profileDF) ) {
          stop( "Not all tags are known in the SMR object or some SMR tags are repeated.", call. = TRUE )
        }
        sparseMatrix( i = inds, j = rep(1,nrow(profileDF)), x = profileDF$Score, dims = c( ncol(smr$M), 1 ) )
      } else {
        if ( length(inds) < nrow(profileDF) ) {
          stop( "Not all tags are known in the SMR object.", call. = TRUE )
        }
        ## tagInds <- which( profileDF$Tag %in% colnames(smr&M)[inds] )
        df <-
          purrr::map_df( 1:nrow(profileDF), function(i) {
            data.frame( Index = which( colnames( smr$M ) %in% profileDF$Tag[i] ),
                        Weight = profileDF$Score[[i]]) } )
        sparseMatrix( i = df$Index, j = rep(1,nrow(df)), x = df$Weight, dims = c( ncol(smr$M), 1 ) )
      }
    } else {
      if ( sum( tagType %in% smr$TagTypes ) == 0 ) {
        stop( "Unknown tag type value for the argument 'tagType'.", call. = TRUE )
      }
      cnames <- colnames(smr$M)[ smr$TagTypeRanges[tagType,"Begin"] : smr$TagTypeRanges[tagType,"End"] ]
      profileDF <- profileDF[ profileDF$Tag %in% cnames, ]
      if ( nrow(profileDF) == 0 ) {
        warning( "None of the given tags belong to the specified tag type. Returning 0.", call. = TRUE )
        return( 0 )
      }
      inds <- which( cnames %in% profileDF$Tag )
      inds <- inds + (smr$TagTypeRanges[tagType,"Begin"] - 1)
      sparseMatrix( i = inds, j = rep(1,nrow(profileDF)), x = profileDF$Score, dims = c( ncol(smr$M), 1 ) )
    }
  } else {
    stop( "Expected a data frame with names c('Score','Index','Tag'), c('Score','Index'), or c('Score','Tag').", call. = TRUE )
  }
}

#' Interpret recommendations
#' @description Gives the interpretation of a data frame of recommendations with sparse matrix recommender object.
#' @param smr A sparse matrix recommender.
#' @param recs A data frame of recommendations with column names "Score", "Index".
#' @param tagTypes A vector of tag types to use.
#' @export
SMRItemData <- function(smr, recs, tagTypes=NULL) {
  if ( is.null(tagTypes) ) {
    sm <- smr$M[recs$Index,]
  } else {
    sm <- smr$M[recs$Index, ]
    sms <- purrr::map( tagTypes, function(tg) sm[,smr$TagTypeRanges[tg, "Begin"]:smr$TagTypeRanges[tg, "End"]] )
    sm <- do.call(cbind, sms)
  }
  pt <- as.data.frame(summary(sm))
  pt <- pt[ order(pt[,1]), ]
  pt[,1]<-rownames(sm)[pt[,1]]
  pt[,2]<-colnames(sm)[pt[,2]]
  names(pt) <- c(names(recs)[[3]], "Metadata", "Weight")
  # Now we can use split(pt, factor(pt$Item))
  unique(pt)
}

#' Tag type correspondence
#' @description Finds the tag type of a tag.
#' @param smr A sparse matrix recommender.
#' @param tag A tag (string) for which we want to find the tag type.
#' @param tag A type ID (string) or NULL.
#' @export
SMRTagType <- function( smr, tag ) {
  if ( is.numeric(tag) || is.integer(tag) ) {
    tagInd <- tag
  } else {
    if ( mean( tag %in% colnames(smr$M) ) == 1 ) {
      ## tagInd <- which( colnames(smr$M) == tag ) does not work when tag is a vector
      ## tagInd <- which( colnames(smr$M) %in% tag ) this would break the order
      tagInd <- pmatch( tag, colnames(smr$M) )
    } else if ( tag %in% rownames(smr$M) ) {
      return(smr$ItemColumnName)
    } else {
      return("None")
    }
  }
  
  if ( length(tagInd) == 1 ) {
    tagTypeInd <- which( smr$TagTypeRanges$Begin <= tagInd & tagInd <= smr$TagTypeRanges$End  )
  } else {
    tagTypeInd <- purrr::map_int( tagInd, function(x) which( smr$TagTypeRanges$Begin <= x & x <= smr$TagTypeRanges$End ) )
  }
  
  if ( length( tagTypeInd ) >= 1 ) {
    smr$TagTypes[ tagTypeInd ]
  } else {
    "None"
  }
}

#' Recommendations reordering
#' @description Re-orders a list of recommendations according to
#' their weighted intersection with a list of tags.
#' @param smr A sparse matrix recommender.
#' @param recs A data frame recommended items, the second column being row names or row indices.
#' @param tagIDs A vector tag ID's of indices with which the recommendations are scored.
#' @details The first column is expected to be of scores.
#' The original Mathematica package function is named \code{InterfaceUserToLoveFiltered}.
#' @family Recommendations functions
#' @export
SMRReorderRecommendations <- function( smr, recs, tagIDs ) {
  if ( is.character( tagIDs ) && length( tagIDs ) > 0 ) {
    ## Assuming column ID's of smr$M
    tagInds <- which( colnames(smr$M) %in% tagIDs )
  } else if ( is.numeric( tagIDs ) && length( tagIDs ) > 0 ) {
    tagInds <- tagIDs
  } else {
    stop( "The third argument, tagIDs, is expected to be a non-empty vector of column indices or column ID's.", call.=TRUE )
  }
  
  profileVec <- sparseMatrix( i=tagInds, j=rep(1,length(tagInds)), x=rep(1,length(tagInds)), dims = c( ncol(smr$M), 1 ) )
  
  newOrder <- smr$M[recs[[2]], ] %*% profileVec
  
  if ( sum( newOrder ) > 0 ) {
    newOrder <- rev( order( as.vector(newOrder) ) )
    recs[ newOrder, ]
  } else {
    recs
  }
}

#' Metadata proofs
#' @description Find the metadata tags that would explain or justify the recommendations.
#' @param smr A sparse matrix recommender.
#' @param toBeLovedItem an ID of a item or its index in \code{smr$M}.
#' @param profile a data frame that is the profile of the customer with columns "Score", "Index", "Tag".
#' @param normalizeScores Should the scores be normalized with \code{max(res$Score)}.
#' @param style Proof style derivation; one of "intersection", "multiplication".
#' @return A data frame with columns names "Score", "Index", "Tag".
#' @family Proof functions
#' @export
SMRMetadataProofs <- function( smr, toBeLovedItem, profile,
                               normalizeScores = TRUE,
                               style = "intersection" ) {
  
  if ( is.null(style) ) {
    style = "intersection"
  }
  
  prodVec <- smr$M[ toBeLovedItem, , drop = FALSE ]
  
  if ( style == "intersection" ) {
    prodVec@x <- rep(1, length(prodVec@x) )
  }
  
  pvec <- SMRProfileDFToVector( smr, profile )
  
  ## SMRProfileDFToVector returns a column vector that is why its result is transposed here
  pvec <- prodVec * t(pvec)
  
  res <- SMRProfileDFFromVector( smr, pvec )
  
  ## guarding a bug where res is a rowless data frame
  if(nrow(res) > 0){
    if (normalizeScores ) {
      res$Score <- res$Score / max(res$Score)
    }
    return( res )
  } else {
    return( NULL )
  }
}

#' History proofs
#' @description Find the items of the history that are the closest to a recommendation.
#' @param smr A sparse matrix recommender.
#' @param toBeLovedItem An ID of a item or its index in \code{smr$M}.
#' @param history A data frame that is the customer purchasing history with columns "Score", <some-item-ID>.
#' @param normalizeScores Should the scores be normalized with \code{max(res$Score)}?
#' @return A data frame with columns names "Score", <some-item-id>.
#' @family Proof functions
#' @export
SMRHistoryProofs <- function( smr, toBeLovedItem, history, normalizeScores=TRUE ) {
  
  # there should be a better way of making sparse matrix or vector
  # from a row of a sparse matrix
  #   prodRow <- smr$M[toBeLovedInd,]
  # Replace with  smr$M[toBeLovedItem,,drop=FALSE]
  prodRow <- smr$M[toBeLovedItem,]
  nzInds <- which( prodRow > 0 )
  prodVec <- sparseMatrix( i=nzInds, j=rep(1,length(nzInds)), x = prodRow[nzInds], dims=c( ncol(smr$M), 1 ) )
  
  vInds <- purrr::map_int( history[,2], function(x) which(rownames(smr$M)==x) )
  scores <- smr$M[ vInds, ] %*% prodVec
  scores <- scores * history[,1]
  
  nzInds <- which( scores > 0 )
  
  # if all scores are zero give a warning and return an empty data frame
  if ( length(nzInds) == 0 ) {
    warning("All scores are zero", call.=TRUE)
    res <- data.frame( Score=numeric(0), Index=integer(0), y=character(0) )
    names(res) <- c("Score", "Index", names(history)[[2]] )
    return(res)
  }
  
  prods <- rownames(smr$M)[vInds][ nzInds ]
  prodInds <- (1:nrow(smr$M))[vInds][ nzInds ]
  scores <- scores[ nzInds ]
  
  res <- as.data.frame( scores );
  res <- cbind( res, prodInds, prods )
  names(res) <- c("Score", "Index", names(history)[[2]] )
  if ( normalizeScores ) {
    if ( as.numeric( t(prodVec) %*% prodVec ) > 0 ) {
      res$Score <- res$Score / ( max(history[,1]) * as.numeric( t(prodVec) %*% prodVec ) )
    } else {
      res$Score <- res$Score / max(res$Score)
    }
  }
  
  res <- res[rev(order(res$Score)),]
  res
}

#' Remove tag types from a sparse matrix recommender
#' @description Creates an SMR object from a given SMR object by removing specified tag types.
#' @param smr A sparse matrix recommender.
#' @param removeTagTypes A list of tag types to be removed from \code{smr}.
#' @return Sparse matrix recommender.
#' @family SMR modification
#' @export
SMRRemoveTagTypes <- function( smr, removeTagTypes ) {
  
  ## Copy of the SMR
  newSMR <- smr
  
  ## There are several ways to do this:
  ## 1. Work with newSMR$TagTypeRanges, take the indices corresponding to tag types not to be removed.
  ## 2. Construct a metadata matrix by taking sub-matrices of the tag types not to be removed.
  pos <- ! ( newSMR$TagTypes %in% removeTagTypes )
  
  applySFs <- SMRCurrentTagTypeSignificanceFactors( newSMR )[pos]
  
  newSMR$M01 <-
    Reduce( function( mat, tt )
      if ( is.null(mat) ) { newSMR$M01[, newSMR$TagTypeRanges[tt,]$Begin : newSMR$TagTypeRanges[tt,]$End ] }
      else { cbind( mat, newSMR$M01[, newSMR$TagTypeRanges[tt,]$Begin : newSMR$TagTypeRanges[tt,]$End ] ) },
      newSMR$TagTypes[pos], NULL )
  newSMR$TagTypeRanges <- newSMR$TagTypeRanges[pos, ]
  newSMR$TagTypes <- newSMR$TagTypes[pos]
  
  widths <- newSMR$TagTypeRanges$End - newSMR$TagTypeRanges$Begin + 1
  ends <- cumsum(widths)
  begins <- ends - widths + 1
  newSMR$TagTypeRanges <- data.frame( Begin=begins, End=ends)
  rownames(newSMR$TagTypeRanges) <- newSMR$TagTypes
  
  newSMR$TagToIndexRules <- setNames( 1:ncol(newSMR$M01), colnames(newSMR$M01) )
  newSMR$ItemToIndexRules <- setNames( 1:nrow(newSMR$M01), rownames(newSMR$M01) )
  
  newSMR$M <- SMRApplyTagTypeWeights( newSMR, applySFs )
  
  newSMR
}


##===========================================================
## Sparse matrix transformations
##===========================================================

#' Convert a data frame of triplets into a sparse matrix
#' @description Turns a data frame of three columns (triplets) into a sparse matrix.
#' @param triplets A data frame with three columns.
#' @return A sparse matrix.
#' @family Sparse matrix transformation functions
#' @export
SMRTripletsToSparseMatrix <-  function( triplets ) {
  itemIDs <- unique( triplets[,1] )
  propertyIDs <- unique( triplets[,2] )
  itemIDToIndex <- 1:length(itemIDs)
  names(itemIDToIndex) <- itemIDs
  propertyIDToIndex <- 1:length(propertyIDs)
  names(propertyIDToIndex) <- propertyIDs
  smat <- sparseMatrix( i=itemIDToIndex[ triplets[,1] ],
                        j=propertyIDToIndex[ triplets[,2] ],
                        x=triplets[,3],
                        dims=c( length(itemIDs), length(propertyIDs) )  )
  rownames(smat) <- itemIDs
  colnames(smat) <- propertyIDs
  
  # I don't think we need the rules arrays. We can always re-create them if needed.
  #list( Matrix=smat, ItemIDToIndex=itemIDToIndex, PropertyIDToIndex=propertyIDToIndex )
  smat
}

#' Convert sparse matrix into triplets
#' @description Converts a sparse matrix into a data frame triplets.
#' @param smat A sparse matrix.
#' @return A data frame of triplets.
#' @family Sparse matrix transformation functions
#' @export
SMRSparseMatrixToTriplets <- function( smat ) {

  triplets <- summary(smat)
  
  if( !is.null(rownames(smat)) ) {
    triplets$i <- rownames(smat)[ triplets$i ]
  }
   
  if( !is.null(colnames(smat)) ) {
    triplets$j <- colnames(smat)[ triplets$j ]
  }

  as.data.frame(triplets, stringsAsFactors=FALSE)
}

#' Impose row ID's
#' @description Makes sure that the rows of a matrix are in 1-to-1 correspondence to an array of row ID's
#' @param rowIDs An array of row ID's.
#' @param smat A matrix with named rows.
#' @return Matrix.
#' @family Sparse matrix transformation functions
#' @export
SMRImposeRowIDs <- function( rowIDs, smat ) {
  
  missingRows <- setdiff( rowIDs, rownames(smat) )
  nMissingRows <- length( missingRows )
  
  if ( nMissingRows > 0 ) {
    # Rows are missing in the matrix
    complMat <- sparseMatrix(i=c(1), j=c(1), x=c(0), dims = c( nMissingRows, ncol(smat) ) )
    
    rownames(complMat) <- missingRows
    colnames(complMat) <- colnames(smat)
    
    smat <- rbind( smat, complMat )
  }
  # At this point each element of rowIDs should have a corresponding row in the matrix
  smat[rowIDs,,drop=FALSE]
}

#' Impose column ID's
#' @description Makes sure that the columns of a matrix are in 1-to-1 correspondence to an array of columns ID's.
#' @param colIDs An array of column ID's.
#' @param smat A matrix with named columns.
#' @return Matrix.
#' @family Sparse matrix transformation functions
#' @export
SMRImposeColumnIDs <- function( colIDs, smat ) {
  
  t( SMRImposeRowIDs( rowIDs = colIDs, smat = t(smat) ) )
}


#' Converts an integer sparse matrix to incidence matrix.
#' @description Replaces each a column of a integer matrix with number of columns
#' corresponding to the integer values.
#' The matrix [[2,3],[1,2]] is converted to [[0,0,1,0,0,0,0,1],[0,1,0,0,0,0,1,0]] .
#' @param mat an integer matrix to be converted to column value incidence matrix.
#' @param rowNamesQ Should the argument row names be the result matrix row names?
#' @param colNamesQ Should the column names derived from the argument matrix 
#' column names be assigned to the result matrix?
#' @export
SMRMakeColumnValueIncidenceMatrix <- function( mat, rowNamesQ = TRUE, colNamesQ = TRUE ) {
  
  tmat <- as( mat, "dgCMatrix")
  df <- summary(tmat)
  df <- data.frame(df)
  # minInt <- min(mat,na.rm = T); maxInt <- max(mat,na.rm = T)
  minInt <- min(tmat@x); maxInt <- max(tmat@x)
  #step <- maxInt - minInt + 1 ## this isincorrect df$j computed as  df$j <- ( df$j - 1 ) * step + df$x
  step <- maxInt + 1
  
  if( min(df$x) < 0 ) {
    warning( "The non-zero values of the matrix are expected to be non-negative integers.", call. = TRUE)
  }
  
  df$j <- ( df$j - 1 ) * step + df$x + 1
  ## In other words we are doing this:
  ## triplets <- ddply( .data = df, .variables = .(i,j),
  ##                   .fun = function(row) { c(row[[1]], (row[[2]]-1)*step + row[[3]] + 1, 1) })
  
  ## Convinient way to check the implmentation:
  ## resMat <- sparseMatrix( i = df$i, j = df$j, x = df$x, dims = c( nrow(mat), ncol(mat)*step ) )
  resMat <- sparseMatrix( i = df$i, j = df$j, x = rep(1,length(df$x)), dims = c( nrow(mat), ncol(mat)*step ) )
  
  if ( rowNamesQ ) { 
    rownames(resMat) <- rownames(mat) 
  }
  
  if ( colNamesQ ) { 
    colnames(resMat) <- as.character(unlist(Map( function(x) { paste(x, 0:maxInt, sep = ".") }, colnames(mat))))
  }
  
  resMat
}


##===========================================================
## Other transformations
##===========================================================

#' Categorize to intervals.
#' @param vec A numerical vector to be categorized.
#' @param probs A numerical vector to be given to 
#' the argument \code{probs} of \code{\link{quantile}}.
#' @param breaks A the breaks to be used.
#' If NULL \code{quantile} is used.
#' @param intervalNamesQ Should the intervals be represented with integers or with character names?
#' @return A character vector.
#' @export
SMRCategorizeToIntervals <- function( vec, breaks = NULL, probs = seq(0,1,0.1), intervalNamesQ = FALSE ) {
  
  if( !is.vector(vec) ) {
    stop( "The first argument vec is expected to be a vector.", call. = TRUE )
  }
    
  if( missing(breaks) || is.null(breaks) ) {
    breaks <- unique( quantile( vec, probs, na.rm = T) )
  }
  
  resVec <- findInterval( x = vec, vec = breaks, all.inside = T )
  
  if( intervalNamesQ ) {
    intervalNames <- purrr::map2_chr( breaks[-length(breaks)], breaks[-1], function(x, y) paste0( x, "≤v<", y )) 
    resVec <- intervalNames[resVec]
  }
  
  resVec
}


##===========================================================
## Class distances functions
##===========================================================

#' Euclidean distances from a vector.
#' @description Computes the Euclidean distances of the rows of a sparse matrix
#' to a specified vector.
#' @param smat A sparse matrix.
#' @param vec A numeric vector.
#' @param method A method for distance computation.
#' One of "cosine", "euclidean".
#' @details The implementation attempts to keep all computational steps
#' in sparse matrix structures. 
#' Note, with "cosine" we mean "cosine distance", not "cosine similarity".
#' @return A numeric vector
#' @export
SMRMatrixDistances <- function( smat, vec = colMeans(smat), method = "euclidean" ) {
  
  if( !( is.numeric(vec) && length(vec) == ncol(smat) ) ) {
    stop( paste0( "The argument vec is expected to a numeric vector with length that equals ncol(smat), ", ncol(smat), "." ), call. = TRUE )
  }
  
  if( tolower(method) %in% c("euclidean", "euclideandistance") ) {
    ## Make the pattern matrix
    smat01 <- smat; smat01@x[ smat01@x != 0 ] <- 1
    
    ## From the pattern matrix and the "mean" vector
    ## Compute the subtraction sparse matrix.
    smatVec <- t( t(smat01) * vec)
    #print( length(smatVec@x) / length(smatVec) )
    
    ## Find the Euclidean-distance residuals.
    vecRes <- sum( vec * vec ) - rowSums( smatVec * smatVec ) 
    
    ## Compute the overall differences.
    m <- ( smat - smatVec )
    m <- rowSums( m * m ) + vecRes
    
    ## Result.
    sqrt(m)
    
  } else if ( tolower(method) %in% c( "cosine", "cosinedistance" ) ) {
    
    smat <- SMRApplyTermWeightFunctions( docTermMat = smat, globalWeightFunction = "None", localWeightFunction = "None", normalizerFunction = "Cosine" )
    
    ## This can be optmized with by making vec a sparse matrix first.
    vec <- vec / sqrt( sum( vec * vec ) ) 
    
    1 - smat %*% vec
    
  } else {
    stop( paste0( "The method ", method, " is uknown." ), call. = TRUE )  
  }
}


#' Euclidean distances from a reference vector per tag.
#' @description 
#' For each tag of \code{tagType} this function does the following steps:
#' 1) finds the sub-matrix of \code{smr$M} for which the corresponding tag column is not zero,
#' 2) finds the mean vector of that sub-matrix,
#' 3) computes the distances of the sub-matrix rows to the mean vector.
#' If \code{tagType = NULL} then for each element of \code{smr$TagTypes}
#' the corresponding sub-matrix distances-to-the-mean are computed.
#' @param smr An SMR object.
#' @param tagType A string that is one of the tag types of \code{smr}.
#' If NULL distances for all tag types are computed.
#' @param method A method for distance computation.
#' One of "cosine", "euclidean".
#' @return A data frame
#' @export
SMRDistances <- function( smr, tagType = NULL, method = "euclidean" ) {
  
  if( !( is.null(tagType) || is.character(tagType) && length(tagType) == 1 && tagType %in% smr$TagTypes ) ) {
    stop( paste0( "The argument tagType is expected to be one of smr$TagTypes, ", smr$TagTypes ), call. = TRUE )
  }
  
  if( is.null(tagType) ) {
    
    dfDists <-
      purrr::map_df( smr$TagTypes, function(tt) {
        SMRDistances( smr = smr, tagType = tt, method = method )
      })
    
  } else {
    
    smat <- SMRSubMatrix( smr, tagType )
    
    dfDists <-
      purrr::map_dfr( colnames(smat), function(x) {
        
        smat <- smr$M[ smr$M[,x] > 0, , drop = F]

        if( nrow(smat) == 0 ) { 
          
          NULL
          
        } else { 
          
          res <- SMRMatrixDistances( smat, colMeans(smat, na.rm = T), method = method )
          
          res <- data.frame( TagType = tagType, Tag = x, Item = rownames(smat), Index = (1:nrow(smr$M))[smr$M[,x] > 0], Distance = res, stringsAsFactors = F)
          
          names(res) <- gsub( "Item", smr$ItemColumnName, names(res) )
          
          res
        }
      })
  }
  
  dfDists
}
  

##===========================================================
## SMR algebra operations
##===========================================================

#' Annex a sub-matrix
#' @description Annex a sub-matrix to the metadata matrix of an SMR object.
#' @param smr A sparse matrix recommender.
#' @param newSubMat The new sub-matrix to be annexed.
#' @param newTagType The tag type associated with the new sub-matrix.
#' @return Sparse matrix recommender.
#' @family SMR modification functions
#' @export
SMRAnnexSubMatrix <- function( smr, newSubMat, newTagType ) {
  
  if ( nrow( newSubMat ) != nrow( smr$M ) ) {
    stop( "The metadata matrix of the SMR object and the new sub-matrix should have the same number of rows.", call. = TRUE )
  }
  
  newSMR <- smr
  
  newSMR$TagTypeRanges <- rbind( newSMR$TagTypeRanges, data.frame( Begin = ncol(newSMR$M) + 1, End = ncol(newSMR$M) + ncol(newSubMat) ) )
  rownames(newSMR$TagTypeRanges) <- c( rownames(newSMR$TagTypeRanges)[-nrow(newSMR$TagTypeRanges)], newTagType )
  
  newSMR$M <- cbind( newSMR$M, newSubMat )
  newSMR$M01 <- cbind( newSMR$M01, newSubMat )
  
  newSMR$TagTypes <- c( newSMR$TagTypes, newTagType )
  
  newSMR
}

#' Join two sparse matrix recommender objects
#' @description Join two SMR objects.
#' @param smr1 The first SMR object.
#' @param smr2 The second SMR object.
#' @param joinType Join type, one of \code{c("inner", "left", "outer", "same")}.
#' @param colnamesPrefix1 The prefix to be concatenated to the colnames of the first SMR object.
#' @param colnamesPrefix2 The prefix to be concatenated to the colnames of the second SMR object.
#' @details The matrices of the given SMR objects are column bound.
#' The argument \code{joinType} says how the rows of the corresponding matrices are changed.
#' For example, if \code{joinType = "outer"} the union of the rownames of SMR matrices is
#' is row-imposed first. See \code{\link{SMRImposeRowIDs}}.
#' @return Sparse matrix recommender.
#' @family SMR modification functions
#' @export
SMRJoin <- function( smr1, smr2, joinType = "same", colnamesPrefix1 = NULL, colnamesPrefix2 = NULL ) {

  if( joinType != "same" ) {
    
    if( joinType == "outer" ) {
      
      allRownames <- unique( c( rownames(smr1$M), rownames(smr2$M) ) )
      
    } else if( joinType == "inner" ) {
      
      allRownames <- intersect( rownames(smr1$M), rownames(smr2$M) )
      
    } else if( joinType == "left") {
      
      allRownames <- rownames(smr1$M)
      
    } else {
      
      stop( paste("Do not know what to do with the joinType value:", joinType ), call. = TRUE )
      
    }
    
    smr1$M <- SMRImposeRowIDs( rowIDs = allRownames, smat = smr1$M )
    smr1$M01 <- SMRImposeRowIDs( rowIDs = allRownames, smat = smr1$M01 )
    
    smr2$M <- SMRImposeRowIDs( rowIDs = allRownames, smat = smr2$M )
    smr2$M01 <- SMRImposeRowIDs( rowIDs = allRownames, smat = smr2$M01 )
    
    SMRJoin( smr1 = smr1, smr2 = smr2, joinType = "same", colnamesPrefix1 = colnamesPrefix1, colnamesPrefix2 = colnamesPrefix2 )
    
  }
  
  if ( nrow( smr1$M ) != nrow( smr2$M ) ) {
    ## The rownames should be the same too.
    stop( "The metadata matrices of the SMR objects have to have the same number of rows.", call. = TRUE )
  }
  
  ## The rownames should be the same too.
  if ( mean( rownames( smr1$M ) == rownames( smr2$M ) ) < 1 ) {
    stop( "The metadata matrices of the SMR objects should have the same rownames.", call. = TRUE )
  }
  
  if ( length( intersect( smr1$TagTypes, smr2$TagTypes ) ) > 0 ) {
    stop( "The tag types of the joined recommenders are expected to be disjoined sets.", call. = TRUE )
  } 
    
  newSMR <- smr1
  
  ranges <- smr2$TagTypeRanges
  ranges$Begin <- ranges$Begin + smr1$TagTypeRanges$End[nrow(smr1$TagTypeRanges)]
  ranges$End <- ranges$End + smr1$TagTypeRanges$End[nrow(smr1$TagTypeRanges)]
  
  newSMR$TagTypeRanges <- rbind( smr1$TagTypeRanges, ranges )
  rownames(newSMR$TagTypeRanges) <- c( paste( colnamesPrefix1, rownames(smr1$TagTypeRanges), sep=""), paste( colnamesPrefix2, rownames(smr2$TagTypeRanges), sep="") )
  
  newSMR$M <- cbind( smr1$M, smr2$M )
  newSMR$M01 <- cbind( smr1$M01, smr2$M01 )
  
  if( length( intersect( colnames(smr1$M), colnames(smr2$M) ) ) != 0 ) {
    if( !is.null(colnamesPrefix1) && is.null(colnamesPrefix2) || colnamesPrefix1 == colnamesPrefix2 ) {
      colnamesPrefix1 <- "1."
      colnamesPrefix2 <- "2."
    }
  }
    
  newSMR$TagTypes <- c( paste( colnamesPrefix1, smr1$TagTypes, sep=""), paste( colnamesPrefix2, smr2$TagTypes, sep="") )
  
  colnames(newSMR$M) <- c( paste0( colnamesPrefix1, colnames(smr1$M) ), paste0( colnamesPrefix2, colnames(smr2$M) ) )
  colnames(newSMR$M01) <- c( paste0( colnamesPrefix1, colnames(smr1$M01) ), paste0( colnamesPrefix2, colnames(smr2$M01) ) )
  
  newSMR
}

#' Row bind a matrix with a sparse matrix recommender
#' @description Row-binds the matrix of a sparse matrix recommender with a sparse matrix.
#' @param smr A sparse matrix recommender.
#' @param smat A sparse matrix.
#' @return A sparse matrix recommender.
#' @family SMR modification functions
#' @export
SMRRowBindMatrix <- function( smr, smat ) {
  
  if ( ncol(smr$M01) == ncol(smat) && mean( colnames(smr$M01) == colnames(smat) ) == 1 ) {
    
    smr$M01 <- rbind( smr$M01, smat )
    
  } else if ( mean( colnames(smat) %in% colnames(smr$M01) ) == 1 ) {
    ## All of the columns of smat are in smr$M01.
    smr$M01 <- rbind( smr$M01, SMRImposeColumnIDs( colIDs = colnames(smr$M01), smat = smat ) )
    
  } else {
    stop( "The column names of the specified sparse matrix are not a subset of the column names of the recommender object.", call. = TRUE )
    return(NULL)
  }
  
  smr$M <- SMRApplyTagTypeWeights( smr = smr, weights = rep(1, length(smr$TagTypes)) )
  
  smr
}

#' Row bind two sparse matrix recommenders
#' @description Returns a sparse matrix recommender object with a matrix
#' that is obtained by row-binding the matrices of two sparse matrix recommenders.
#' @param smr1 The first SMR object.
#' @param smr2 The second SMR object.
#' @return Sparse matrix recommender.
#' @family SMR modification
#' @export
SMRRowBind <- function( smr1, smr2 ) {
  
  if( ncol(smr1$M01) == ncol(smr2$M01) && mean( colnames(smr1$M01) == colnames(smr2$M01) ) == 1 ) {
    
    SMRRowBindMatrix( smr1, smr2$M01 )
    
  } else if ( length(smr1$TagTypes) == length(smr2$TagTypes) && mean( smr1$TagTypes == smr2$TagTypes ) == 1  ) {
    
    smats <-
      purrr::map( smr1$TagTypes, function(tt) {
        smat1 <- SMRSubMatrix( smr = smr1, tagType = tt)
        smat2 <- SMRSubMatrix( smr = smr2, tagType = tt)
        colIDs <- unique( c(colnames(smat1), colnames(smat2)) )
        smat1 <- SMRImposeColumnIDs( colIDs = colIDs, smat = smat1 )
        smat2 <- SMRImposeColumnIDs( colIDs = colIDs, smat = smat2 )
        rbind(smat1, smat2)
      })
    
    SMRCreateFromMatrices( matrices = smats, tagTypes = smr1$TagTypes, itemColumnName = smr1$ItemColumnName )
    
  } else {
    stop( "The tag types of the SMR objects to be row-bound are not the same.", call. = TRUE)
    return(NULL)
  }
  
}

##===========================================================
## Transformations to data frames
##===========================================================

#' Sub-matrix to data frame conversion
#' @description Makes a data frame of the sparse matrix recommender sub-matrix
#' that corresponds to a tag type.
#' @param smr A sparse matrix recommender.
#' @param tagType A tag type (string).
#' @return A data frame.
#' @export
SMRSparseMatrixToDF <- function( smr, tagType  ) {
  
  if( !(tagType %in% smr$TagTypes) ) {
    stop("The parameter tagType is not of the tag types of the SMR object.")
  }
  
  smat <- SMRSubMatrix( smr = smr, tagType = tagType )
  df <- summary(smat)
  df <- df[ df$x > 0, ]
  
  df <- data.frame(  Rownames = rownames(smat)[df$i], Colnames = colnames(smat)[df$j], Weight = df$x, stringsAsFactors = FALSE )
  
  names(df) <- c( smr$ItemColumnName, tagType, "Weight")
  df
}

#' Sub-matrices to data frame long form conversion
#' @description Long form of the data frame.
#' @param smr A sparse matrix recommender.
#' @param tagTypes A vector tag types (strings) to make the data frame with.
#' @return A data frame.
#' @export
SMRMatricesToLongDF <- function( smr, tagTypes = NULL ) {
  
  if ( is.null(tagTypes) ) { tagTypes = smr$TagTypes }
  
  dfs <-
    purrr::map( tagTypes, function(tt) {
      df <- SMRSparseMatrixToDF(smr, tt)
      if ( nrow(df) == 0 ) { NULL }
      else {
        names(df) <- c( smr$ItemColumnName, "Value", "Weight")
        cbind(df, TagType = tt, stringsAsFactors = FALSE )
      }
    } )
  
  dfs <- dfs[ !is.null(dfs) ]
  do.call( rbind, dfs )
}

#' Sub-matrices to data frame wide form conversion
#' @description Long form of the data frame.
#' @param smr A sparse matrix object.
#' @param tagTypes The tag types to make the data frame with.
#' @return A data frame.
#' @export
SMRMatricesToWideDF <- function( smr, tagTypes = NULL, sep = ", ", .progress = "none" ) {
  df <- SMRMatricesToLongDF( smr, tagTypes )
  dfCast <- reshape2::dcast( data = df,
                             formula = as.formula( paste( smr$ItemColumnName, " ~ TagType " ) ),
                             value.var = "Value", fun.aggregate = function(x) paste(x, collapse = sep ) )
}



##===========================================================
## Overloading predict
##===========================================================

#' Classification with a sparse matrix recommender object
#' @description Classify a data frame or matrix using a sparse matrix recommender.
#' @param smr A sparse matrix recommender.
#' @param data A matrix or a data frame.
#' @param type What kind of result to be returned?
#' The value 'raw' returns a matrix, 'decision' a vector of labels.
#' @param normalizedQ Should the results be normalized or not if type equals 'raw'?
#' @param ... Parameters \code{\link{SMRClassifyByProfileVector}}.
#' @details The sparse matrix recommender \code{smr} can have additional parameters
#' tucked-in to \code{smr['ClassifierParameters']}.
#' The list \code{smr['ClassifierParameters']} can have elements: 
#' "tagType", "nTopNNs", "voting", "dropZeroScoredLabels". (Same as \code{...}.)
#' @return If \code{type="decision"} -- a vector of scored class labels.
#' If \code{type="raw"} or \code{type="scores"} -- a contingency matrix of labels and scores.
#' @export
predict.SMR <- function( smr, data, type = "decision", normalizedQ = TRUE, ... ) {
  
  if( !( is.data.frame(data) || is.matrix(data) || ( "dgCMatrix" %in% class(data) ) ) ) {
    stop( "The argument data is expected to be a matrix or a data frame.", call. = TRUE )
  }
  
  if( is.data.frame(data) ) {
    dataMat <- SMRCreate( dataRows = data, tagTypes = setdiff( colnames(data), smr$ItemColumnName), itemColumnName = smr$ItemColumnName )
    dataMat <- dataMat$M
  } else if ( is.matrix(data) ) {
    dataMat <- as( data, "sparseMatrix" )
  } else {
    ## Should be "dgCMatrix".
    dataMat <- data
  }

  ## There should be a check is dataMat a sparse matrix.
  dataMat <- SMRImposeColumnIDs( colIDs = colnames(smr$M), smat = dataMat )
  
  dotArgs <- list(...)
  
  clParams <- if( "ClassifierParameters" %in% names(smr) ) { smr["ClassifierParameters"] } else { NULL }
  
  tagType <- if( "tagType" %in% names(dotArgs) ) { dotArgs[["tagType"]] }
  else if( "tagType" %in% names(clParams) ) { clParams[["tagType"]] }
  else { smr$TagTypes[[length(smr$TagTypes)]] }
  
  nTopNNs <- if( "nTopNNs" %in% names(dotArgs) ) { dotArgs[["nTopNNs"]] }
  else if( "nTopNNs" %in% names(clParams) ) { clParams[["nTopNNs"]] }
  else { 20 }
  
  voting <- if( "voting" %in% names(dotArgs) ) { dotArgs[["voting"]] }
  else if( "voting" %in% names(clParams) ) { clParams[["voting"]] }
  else { FALSE }
  
  dropZeroScoredLabels <- if( "dropZeroScoredLabels" %in% names(dotArgs) ) { dotArgs[["dropZeroScoredLabels"]] }
  else if( "dropZeroScoredLabels" %in% names(clParams) ) { clParams[["dropZeroScoredLabels"]] }
  else { TRUE }
  
  if( tolower(type) == "decision" ) {
    
    purrr::map_chr( 1:nrow(data), function(i) {
      
      pvec <- dataMat[i,,drop=F]
      
      recs <- SMRClassifyByProfileVector( smr = smr, tagType = tagType, profileVec = pvec,
                                          nTopNNs = nTopNNs, voting = voting,
                                          dropZeroScoredLabels = dropZeroScoredLabels)
      
      if( length(recs) == 0 || is.null(recs)) { NA } else { as.character(recs$Label)[[1]] }
      
    } )
    
  } else if ( tolower(type) %in% c( "raw", "scores" ) ) {
    
    res <-
      purrr::map_df( 1:nrow(data), function(i) {
        
        pvec <- dataMat[i,,drop=F]
        
        recs <- SMRClassifyByProfileVector( smr = smr, tagType = tagType, profileVec = pvec,
                                            nTopNNs = nTopNNs, voting = voting,
                                            dropZeroScoredLabels = dropZeroScoredLabels)

        if( normalizedQ && sum(recs$Score) > 0 ) { recs$Score <- recs$Score / sum(recs$Score)}
        
        cbind( Index = i, recs )
      } )
    
    as.matrix( xtabs( Score ~ Index + Label, res, sparse = T ) )
  } else {
    stop( "The argument type is expected to be one of 'decision', 'raw', 'scores'.", call. = TRUE )
  }
  
}

#=======================================================================================
# Object-Oriented Programming (OOP) implementations
#=======================================================================================


##===========================================================
## Generic function definition
##===========================================================

## Note that in the functions below the data frames with the recommendations results have (only) the columns "Score" and "Item".
## The more basic recommendations functions return data frames that also have the column "Index", but the indices are not invariant
## across the recommenders. The item names are.

#' Compute recommendations
#' @description The generic function for calculating recommendations by history.
#' @param x A recommender object.
#' @param historyItems A list of history items (indices or ID's).
#' @param historyRatings A list of history ratings.
#' @param nrecs Number of required recommendations.
#' @param removeHistory Should the history be dropped or not?
#' @return A data frame with the columns c("Score", "Item").
#' @family Recommendations calculation functions
#' @export
Recommendations <- function( x, historyItems, historyRatings, nrecs, removeHistory = TRUE, ... ) UseMethod( "Recommendations" )

#' Specialization of Recommendations for SMR objects
#' @description Specialization of Recommendations for SMR objects.
#' @param x A recommender object.
#' @param historyItems A list of history items (indices or ID's).
#' @param historyRatings A list of history ratings.
#' @param nrecs Number of required recommendations.
#' @param removeHistory Should the history be dropped or not?
#' @return The result is a data frame with the columns c("Score", "Item").
#' @family Recommendations calculation functions
#' @export
Recommendations.SMR <- function( x, historyItems, historyRatings, nrecs, removeHistory = TRUE, ... ) {
  ## Needs handling of the argument tuningParametes.
  res <- SMRRecommendations( smr = x, userHistoryItems = historyItems, userRatings = historyRatings,
                             nrecs = nrecs, removeHistory = removeHistory )
  setNames( res[, c(1,3)], c("Score", "Item") )
}

#' Compute recommendations by profile
#' @description The generic function for calculating recommendations by profile.
#' @param x A recommender object.
#' @param profileTags A list of profile tags.
#' @param profileTagScores A list of scores corresponding to the profile tags.
#' @param nrecs Number of required recommendations.
#' @return A data frame with the columns columns c("Score", "Item")
#' @export
RecommendationsByProfile <- function( x, profileTags, profileTagScores, nrecs, ... ) UseMethod( "RecommendationsByProfile" )

#' Specialization of \code{RecommendationsByProfile} for SMR objects
#' @description Specialization of RecommendationsByProfile for SMR objects.
#' @param x A recommender object.
#' @param profileTags A list of profile tags.
#' @param profileTagScores A list of scores corresponding to the profile tags.
#' @param nrecs Number of required recommendations.
#' @return A data frame with the columns columns c("Score", "Item").
#' @export
RecommendationsByProfile.SMR <- function ( x, profileTags, profileTagScores, nrecs, ... ) {
  ## Needs handling of the argument tuningParametes.
  res <- SMRRecommendationsByProfileDF( smr = x,
                                        profile = data.frame( Score = profileTagScores, Tag = profileTags, stringsAsFactors=FALSE),
                                        nrecs = nrecs )
  res[, c("Score", "Item")]
}

#' Calculation of a consumption profile
#' @description Generic function for calculating a consumption profile.
#' @param x a recommender object
#' @param historyItems a list of history items (indices or ID's)
#' @param historyRatings a list of history ratings
#' @param allColumns a logical are all columns of the results returned or not
#' @return A data frame with the first columns being "Score" and "Tag".
#' @export
ConsumptionProfile <- function( x, historyItems, historyRatings, allColumns = FALSE, ... ) UseMethod( "ConsumptionProfile" )

#' Calculation of a consumption profile with an SMR object
#' @description Specialization function for calculating a consumption profile.
#' @param x a recommender object
#' @param historyItems a list of history items (indices or ID's)
#' @param historyRatings a list of history ratings
#' @param allColumns a logical are all columns of the results returned or not
#' @return A data frame with the first columns being "Score" and "Tag".
#' @export
ConsumptionProfile.SMR <- function( x, historyItems, historyRatings, allColumns = FALSE, ... ) {
  if( missing(historyRatings) || is.null(historyRatings) ) {
    historyRatings <- rep( 1, length(historyItems) )
  }
  if( allColumns ) {
    SMRProfileDF( x, data.frame( Rating = historyRatings, Item = historyItems, stringsAsFactors = FALSE ) )[, c("Score", "Tag", "Index")]
  } else {
    SMRProfileDF( x, data.frame( Rating = historyRatings, Item = historyItems, stringsAsFactors = FALSE ) )[, c("Score", "Tag")]
  }
}

#' Classification using a profile vector
#' @description Classify a profile vector into the column names of a tag type sub-matrix.
#' @param x A recommender object.
#' @param tagType Tag type for which the classification is done.
#' @param profileVec Is a sparse matrix with 1 row (a row from a sparse matrix).
#' @param nTopNNs Number of top nearest neighbors to be used in to derive the classification.
#' @param voting Should simple voting be used or a weighted sum?
#' @return A list of named scores.
#' @export
ClassifyByProfileVector <- function( x, tagType, profileVec, nTopNNs, voting = FALSE ) UseMethod( "ClassifyByProfileVector" )

#' Classification using a profile vector
#' @description Specialization of \code{\link{ClassifyByProfileVector}}
#' for sparse matrix recommender objects.
#' Classify a profile vector into the column names of a tag type sub-matrix.
#' @param x A recommender object.
#' @param tagType Tag type for which the classification is done.
#' @param profileVec Is a sparse matrix with 1 row (a row from a sparse matrix).
#' @param nTopNNs Number of top nearest neighbors to be used in to derive the classification.
#' @param voting Should simple voting be used or a weighted sum?
#' @return A list of named scores.
#' @export
ClassifyByProfileVector.SMR <- function ( x, tagType, profileVec, nTopNNs, voting = FALSE ) {
  SMRClassifyByProfileVector( smr = x, tagType = tagType, profileVec = profileVec, nTopNNs = nTopNNs, voting = voting )
}

##===========================================================
## Composite pattern for recommenders combination
##===========================================================
## Here is way to construct a composite recommender object:

# rcObj <- list( Recommenders = list( "SMR1" = smr1, "SMRFreq1" = smrFreq1, "SMR2" = smr2, "SMR3" = smr3 ), Weights = c(1,0.5,1,1),
#               NormalizationType = "quantileIntervals", MergeFunction = length )
# class(rcObj) <- "CompositeRecommender"

#' Calculate recommendations with a composite recommender
#' @description Calculate recommendations over a composite recommender object.
#' @param x A recommender object.
#' @param historyItems A list of history items.
#' @param historyRatings A list of history ratings.
#' @param nrecs Number of required recommendations.
#' @param removeHistory Should the history be removed or not?
#' @param normalizationType Normalization type, one of NULL, 'none', 'max', 'rank', 'quantileIntervals',
#' or 'shiftAndRescale' (same as NULL).
#' @param mergeFunction A function to merge the recommendations lists, a function that can be applied to
#' a vector of scores corresponding to an item.
#' @details If the argument \code{normalizationType} is NULL, then the object's element 'NormalizationType' is used.
#' If that is NULL too, then 'shiftAndRescale' is used. Examples of values of \code{mergeFunction} are 'sum',
#' 'max', 'mean', 'median', 'length'. If \code{mergeFunction} is NULL, then the object's element 'MergeFunction'
#' is used. If that is NULL too, then sum is used.
#' @return A data frame with the columns columns "Score", "Item".
#' @export
Recommendations.CompositeRecommender <- function( x, historyItems, historyRatings, nrecs, removeHistory = TRUE,
                                                  normalizationType = NULL, mergeFunction = NULL, ... ) {
  
  ## Computing recommendations with each recommender
  allRecs <- 
    purrr::map( x$Recommenders, function(recObj) {
      recs <- Recommendations( recObj,
                               historyItems = historyItems,
                               historyRatings = historyRatings,
                               nrecs = nrecs,
                               removeHistory = removeHistory, ... ) 
      if( !is.null(recObj$ItemColumnName) ) {
        names(recs) <- gsub( recObj$ItemColumnName, "Item", names(recs) )
      }
      recs
    })
  
  ## Determine weights for the recommenders
  weights <- x$Weights
  if ( is.null( weights ) ) { weights <- rep(1, length( x$Recommenders ) ) }
  if ( length( weights ) < length( x$Recommenders ) ) { weights <- rep_len( weights, length.out = length( x$Recommenders ) ) }
  
  ## Default normalizationType if NULL
  if ( is.null( normalizationType ) ) { normalizationType <- x$NormalizationType }
  if ( is.null( normalizationType ) ) { normalizationType <- "shiftAndRescale" }
  
  ## Default mergeFunction if NULL
  if ( is.null( mergeFunction ) ) { mergeFunction <- x$MergeFunction }
  if ( is.null( mergeFunction ) ) { mergeFunction <- sum }
  
  ## Normalization of scores
  ## Weights for the different recommenders can be used.
  if ( normalizationType == "max" ) {
    
    allRecsDF <- purrr::map_df( 1:length(allRecs), function(i) { x <- allRecs[[i]]; x$Score <- weights[i] * ( x$Score / max(x$Score) ); x } )
    
  } else if ( normalizationType == "rank" ) {
    
    maxNRow <- max( purrr:map_int( allRecs, nrow ) )
    allRecsDF <- purrr::map_df( 1:length(allRecs), function(i) { x <- allRecs[[i]]; x$Score <- weights[i] * ( maxNRow - (0:(nrow(x)-1)) ); x } )
    
  } else if ( normalizationType == "quantileIntervals" ) {
    
    ## Note that here are handled quantile levels "probs" if given as an argument.
    args <- list(...)
    if ( !("probs" %in% names(args)) ) { probs <- seq(0,1,0.2) }
    
    allRecsDF <-
      purrr::map_df( 1:length(allRecs), function(i) {
        x <- allRecs[[i]]
        qs <- quantile( x$Score, probs, na.rm = TRUE )
        x$Score <- weights[i] * findInterval( x = x$Score, vec = qs )
        x
      } )
    
  } else if ( normalizationType == "shiftAndRescale" ) {
    
    ## May be just using scale would suffice.
    ## Note that bottom outliers are removed.
    allRecsDF <-
      purrr::map_df( 1:length(allRecs), function(i) {
        x <- allRecs[[i]]
        qs <- quantile( x$Score, seq(0,1,0.25), na.rm = TRUE );
        if ( qs[4] - qs[2] > 0 ) {
          x$Score <- ( x$Score - qs[3] ) / ( qs[4] - qs[2] ) / 2 + 1
        }
        x$Score[ x$Score < 0 ] <- 0
        x$Score <- weights[i] * x$Score
        x
      })
    
  } else if ( normalizationType == "none" ) {
    
    allRecsDF <- do.call( rbind, allRecs )
    
  } else {
    stop( "The argument 'normalizationType' is not one of: NULL, 'none', 'max', 'rank', 'quantileIntervals', 'shiftAndRescale'.", call. = TRUE )
  }
  
  allRecsDF <- allRecsDF[ allRecsDF$Score > 0, ]
  gsub
  ## Note, the merging here is with merge-sum. Other merging can be applied.
  res <- ddply( allRecsDF, "Item", function(x) { data.frame( Score = mergeFunction(x$Score), Item = x$Item[1], stringsAsFactors = FALSE ) } )
  res <- res[ order(-res$Score), ]
  
  res
}


##===========================================================
## Recommenders items and tags query methods
##===========================================================

#' Recommender tags
#' @param recommender A recommender object.
#' @return A list of tags.
#' @export
RecommenderTags <- function( recommender )  UseMethod("RecommenderTags")
RecommenderTags.SMR <- function( recommender ) colnames( recommender$M )
RecommenderTags.CompositeRecommender <- function( recommender ) unique( unlist( purrr::map( recommender$Recommenders, RecommenderTags ) ) )

#' Recommender items
#' @param recommender A recommender object.
#' @return A list of item ID's (strings).
#' @export
RecommenderItems <- function( recommender ) UseMethod("RecommenderItems")
RecommenderItems.SMR <- function( recommender ) rownames( recommender$M )
RecommenderItems.CompositeRecommender <- function( recommender ) unique( unlist( purrr::map( recommender$Recommenders, RecommenderItems ) ) )


