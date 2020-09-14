##=======================================================================================
## Implementation of a Hub-Item Dynamic Ranking Algorithm in R
## Copyright (C) 2015-2019  Anton Antonov
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
## Written by Anton Antonov,
## antononcube @ gmail . com,
## Windermere, Florida, USA.
##
##=======================================================================================
## R is a language and environment for statistical computing and
## graphics. It is a GNU project which is similar to the S language
## and environment which was developed at Bell Laboratories (formerly
## AT&T, now Lucent Technologies) by John Chambers and colleagues. R
## can be considered as a different implementation of S.
##
## R is available as Free Software under the terms of the Free
## Software Foundation's GNU General Public License in source code
## form.
##
## For more details see http://www.r-project.org/ .
##
## =======================================================================================
## The Hub-Item Dynamic Ranking Algorithm (HIDRA) can be seen as an
## extension of PageRank.  See http://en.wikipedia.org/wiki/PageRank .
## The difference is that HIDRA works on a bi-partite graph and
## multiple rank vectors are calculated using different biasing based on
## predicates over the nodes. For a particular query a linear
## combination of the closest ranks vectors is used in order to obtain
## the response ranks. (That is the dynamic ranking part.)
##
## There are at least two other similar algorithms:
## 1. Hyperlink-Induced Topic Search (HITS), http://en.wikipedia.org/wiki/HITS_algorithm, and
## 2. Topic-Sensitive PageRank, http://en.wikipedia.org/wiki/Topic-Sensitive_PageRank .
##=======================================================================================
## Version 0.4
## This implementation started as re-implementation of a Mathematica implementaion of HIDRA.
## At some point it became better and more useful than the Mathematica one.
##=======================================================================================
## TODO
## 1. Explanation of the flow of computations and how to be used.
## 2. Explanations why Power Method is used over the whole matrix instead on
##    the matrix hub-item and item-hub blocks. (As in HITS.)
## 3. Review of the dynamic ranking queries functions.
##=======================================================================================

#' @import dplyr
#' @import purrr
#' @import reshape2
#' @import Matrix
NULL


##===========================================================
## Vector norm
##===========================================================

#' Vector norm
#' @description Find the Euclidean norm of a vector.
#' @param x A numeric vector.
#' @param type Type of norm.
#' One of: 'Euclidean', \code{2}, 'Infinity', \code{Inf}, 'One', code{1}.
#' @export
VectorNorm <- function(x, type = "euclidean") {
  if( type == 2 || tolower(type) %in% c( "euclidean", "two" ) ) {
    sqrt(sum(x*x))
  } else if( type == Inf || tolower(type) %in% c( "infinity", "inf" ) ) {
    max(abs(x))
  } else if( type == 1 || tolower(type) == "one" ) {
    sum(abs(x))
  } else {
    stop( "Unknown norm type. The argument type is expected to be one of: 'Euclidean', 2, 'Infinity', Inf, 'One', 1.", call. = TRUE )
  }
}


##===========================================================
## Power Method
##===========================================================

#' Power method eigenvector computation.
#' @description Find the eigenvector with the largest eigenvalue using the Power method.
#' @param mat A sparse matrix.
#' @param bvec A biasing vector (a guess of the probabilities).
#' @param alpha The significance of \code{mat} in the iteration formula.
#' @param maxSteps The maximum number of iteration steps.
#' @details The iteration formula is:
#' \code{alpha * mat %*% v + ( 1 - alpha ) * ( bvec * sum(v) )}.
#' @export
PowerMethod <- function( mat, bvec, alpha, maxSteps = 100, tol = 10^(-6) ) {

  n <- ncol(mat)
  v <- runif( n = n, min = 0, max = 1 )
  v <- v / max(v)
  v <- sparseMatrix( i=1:n, j=rep(1,n), x=v )
  k <- 0

  vold <- sparseMatrix( i=1:n, j=rep(1,n), x=rep(1,n) )
  if ( is.numeric(bvec) ) {
    bvec <- sparseMatrix( i=1:n, j=rep(1,n), x=bvec )
  }

  while ( norm( v - vold, "F" ) > tol && k < maxSteps ) {
    vold <- v
    v <- alpha * mat %*% v + ( 1 - alpha ) * ( bvec * sum(v) )
    v <- v / max(abs(v))
    k <- k + 1
  }

  rownames(v) <- rownames(mat)
  list( Vector = v, Iterations = k, ResidualNorm = norm( v - vold, "F" ) )
}


##===========================================================
## Make bi-partite graph matrix
##===========================================================

#' Make bi-partite graph matrix.
#' @description Makes the adjacency matrix of a bi-partite graph connecting.
#' @param hubItemScoresArray A data frame or data table with columns...
#' @param hubIDColName Hub ID column name.
#' @param itemIDColName Item ID column name.
#' @param hubItemScoreColName The column name of a hub-to-item scores; if NULL contingency values are used.
#' @param itemHubScoreColName The column name of a item-to-hub scores; if NULL contingency values are used.
#' @param columnStochasticQ Should the resulting matrix be made column stochastic?
#' @return A list of three elements: matrix M, hub-to-index data frame Hubs, item-to-index data frame Items.
#' @export
MakeBiPartiteGraphMatrix <- function( hubItemScoresArray,
                                      hubIDColName = "HubID", itemIDColName  = "ItemID",
                                      hubItemScoreColName = NULL, itemHubScoreColName  = NULL,
                                      columnStochasticQ = TRUE ) {

  if ( sum( class(hubItemScoresArray) %in% "data.frame" ) == 0 ) {
    stop( "The argument hubItemScoresArray is expected to be a data frame or a data table.", call. = TRUE )
  }

  if ( is.null(hubItemScoreColName) ) {
    bmat1 <- xtabs( as.formula( paste( hubItemScoreColName, "~", hubIDColName, "+", itemIDColName ) ), hubItemScoresArray, sparse=TRUE )
  } else {
    bmat1 <- xtabs( as.formula( paste( "~", hubIDColName, "+", itemIDColName ) ), hubItemScoresArray, sparse=TRUE )
  }

  if ( is.null(itemHubScoreColName) ) {
    bmat2 <- xtabs( as.formula( paste( "~", itemIDColName, "+", hubIDColName ) ), hubItemScoresArray, sparse=TRUE )
  } else {
    bmat2 <- xtabs( as.formula( paste( itemHubScoreColName, "~", itemIDColName, "+", hubIDColName ) ), hubItemScoresArray, sparse=TRUE )
  }

  hhZeroMat <- sparseMatrix( i = c(1), j = c(1), x=c(0), dims = c( nrow(bmat1), nrow(bmat1) ) )
  iiZeroMat <- sparseMatrix( i = c(1), j = c(1), x=c(0), dims = c( ncol(bmat1), ncol(bmat1) ) )
  bmat <- rBind( cBind( hhZeroMat, bmat1 ), cBind( bmat2, iiZeroMat ) )

  # Make the matrix column stochastic.
  if ( columnStochasticQ ) {
    colNorms <- sqrt( colSums( bmat * bmat ) )
    bmat <- bmat %*% Diagonal( x = 1 / ifelse( colNorms > 0, colNorms, 1) )
  }

  hubDF <- data.frame( HubID = rownames(bmat1), Index = 1:nrow(bmat1), stringsAsFactors = FALSE )
  itemDF <- data.frame( ItemID = rownames(bmat2), Index = (1:nrow(bmat2)) + nrow(bmat1), stringsAsFactors = FALSE )

  rownames(bmat) <- c( rownames(bmat1), rownames(bmat2) )
  colnames(bmat) <- c( rownames(bmat1), rownames(bmat2) )

  list( M = bmat, Hubs = hubDF, Items = itemDF )
}


##===========================================================
## Hub-item ranks
##===========================================================

#' Hub-item ranks.
#' @description Calculate the hub-item ranks for a given adjacency matrix of bi-partite graph and characterizing tags of the nodes.
#' @param hubsAndItemsMat A bipartite graph adjacency matrix of hub-item connections
#' with rownames and colnames corresponding to the hub and item ID's.
#' @param hubIDs A data frame with columns \code{c('HubID','HubIndex')} mapping ID's to indexes in \code{hubsAndItemsMat}.
#' @param hubTags A list of tags lists, each characterizing its corresponding hub in \code{hubIDs}.
#' @param tagSets A list of (frequent) metadata sets.
#' @param tagPresenceFraction What fraction of each tag set is in \code{hubTags}?
#' @param bias A numerical vector with biasing values for each hub and item.
#' @param normalizationFunction A normalization function applied to the ranks vector for each tag set.
#' @return A list of sparse arrays corresponding to the item ranks for \code{tagSets}.
#' @export
HubItemRanks <- function( hubsAndItemsMat, hubIDs, hubTags, tagSets,
                          tagPresenceFraction = 0.8, bias = NULL, normalizationFunction = max,
                          maxSteps = 100, sumFactor = 0.2, numberOfHubRanks = 200, numberOfItemRanks = 200 ) {

  ## Hubs are assumed to be in the front of the matrix
  if( !is.data.frame(hubIDs) ) {
    stop( "The argument hubIDs is expected to be a data frame with columns c('HubID','HubIndex').", call. = TRUE )
  }

  # preliminary
  n <- ncol(hubsAndItemsMat)

  # find eigenvectors
  rankVecs <- purrr::map( tagSets,
                          function(ts) {

                            if ( length(ts) == 1 ) {
                              hubInds <- purrr::map_int( hubTags, function(x) length( intersect( ts, x ) ) )
                              hubInds <- hubInds > 0
                              hubInds <- (1:length(hubTags))[ hubInds ]
                            } else {
                              hubInds <- purrr:map_lgl( hubTags, function(x) length( intersect( ts, x ) ) / length(ts) >= tagPresenceFraction )
                              hubInds <- (1:length(hubTags))[ hubInds ]
                            }

                            # hubsAndItemsMat is a square matrix.
                            bvec <- rep(0.0, n ); names(bvec) <- rownames(hubsAndItemsMat)
                            bvec[ hubIDs$HubIndex[hubInds] ] <- 1

                            if ( !is.null(bias) ) {
                              bvec <- bvec * bias
                              if( max(bvec) > 0 ) { bvec <- bvec / max(bvec) }
                            }

                            ## Find the ranks.
                            res <- PowerMethod( hubsAndItemsMat, bvec, sumFactor, maxSteps=maxSteps)

                            ## Normalize.
                            uEVec <- res$Vector
                            if ( !is.null(normalizationFunction) ) {
                              uEVec <- uEVec / normalizationFunction(uEVec)
                            }

                            ## Find the threshold for hubs.
                            ranksPerVec <- min( nrow(hubIDs), numberOfHubRanks )
                            vecTh <- sort(uEVec@x[1:nrow(hubIDs)])[nrow(hubIDs)-ranksPerVec+1]

                            ## Remove the zeroes from the sparse pattern obtained with uEVec@x[ uEVec@x > vecTh ] <- 0.
                            pickInds <- (uEVec@x >= vecTh) & ( (1:n) <= nrow(hubIDs) )
                            hubsVec <- sparseMatrix( i = (1:n)[pickInds], j = rep(1, sum(pickInds) ), x = uEVec@x[pickInds], dims = c(n,1) )
                            rownames(hubsVec) <- rownames(hubsAndItemsMat) ## should be equal to rownames(res$Vector)

                            ## Find the threshold for items
                            ranksPerVec <- min( n-nrow(hubIDs), numberOfItemRanks )
                            vecTh <- sort(uEVec@x[(nrow(hubIDs)+1):n])[(n-nrow(hubIDs))-ranksPerVec+1]

                            ## Remove the zeroes from the sparse pattern obtained with uEVec@x[ uEVec@x > vecTh ] <- 0.
                            pickInds <- (uEVec@x >= vecTh) & ( (1:n) > nrow(hubIDs) )
                            itemsVec <- sparseMatrix( i = (1:n)[pickInds], j = rep(1, sum(pickInds) ), x = uEVec@x[pickInds], dims = c(n,1) )
                            rownames(itemsVec) <- rownames(hubsAndItemsMat) ## should be equal to rownames(res$Vector)

                            ## Result.
                            list( HubRanks = hubsVec, ItemRanks = itemsVec )
                          }
  )

  purrr::map( 1:length(tagSets), function(i) list( TagSet=tagSets[[i]], HubRankVector=rankVecs[[i]]$HubRanks, ItemRankVector=rankVecs[[i]]$ItemRanks ) )

}


##===========================================================
## Nearest tag sets
##===========================================================

#' Nearest tag sets.
#' @description Finds the closest lists of strings to a list of strings.
#' @param tagSets A list of lists of strings.
#' @param tset A list of strings.
#' @return A list of lists of strings.
#' @export
NearestTagSets <- function(tagSets, tset) {
  ds <- dlply( tagSets, function(x) c( length(intersect( tset, x )) / length(x), x ) )
  names(ds) <- c("Score","TagSet")
  ds <- ds[ rev(order(ds$Score)) ]
  if ( ds$Score[1] == 0 ) {
    NULL
  } else {
    ds[ ds$Score == ds$Score[1], ]
  }
}


# Dynamic rank.
# @description Find the dynamic ranks.
# @param tagSet
# @param tagSetAndRankVectorPairs
# @export
# DynamicRank <- function(tagSet, tagSetAndRankVectorPairs ) {
#   s <- NearestTagSets( llply( tagSetAndRankVectorPairs, function(x) x[[1]] ), tagSet )
#   if ( is.null(s) ) {
#     rep( 0, tagSetAndRankVectorPairs[[1]][[2]] )
#   } else {
#
#   }
# }
