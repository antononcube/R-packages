

#' @import SparseMatrixRecommender
#' @import SMRMon
#' @import Matrix
#' @import magrittr
#' @import purrr
#' @import dplyr
#' @import stringi
#' @import jsonlite
NULL


##===========================================================
## Make Test By Recommendation
##===========================================================

#' Make a test by recommendation
#' @description Makes a recommendation test for given SMR object and profile.
#' @param smr An SMR object.
#' @param profile A profile. Has a form accepted by \code{\link{SMRMonRecommendByProfile}}.
#' @param nrecs Number of recommendations.
#' @param tagType The tag type to assign to the test.
#' @param ... Additional arguments for \code{\link{SMRMonRecommendByProfile}}.
#' @return A data frame.
#' @export
MakeTestByRecommendation <- function( smr, profile, nrecs, tagType = smr$ItemColumnName, ...) {

  if( ! SMRSparseMatrixRecommenderQ(smr) ) {
    stop( "The argument smr is expected to be a SMR object.", call. = TRUE )
  }

  if( !( is.null(nrecs) || is.numeric(nrecs) && length(nrecs) ==1 && nrecs > 0) ) {
    stop( "The argument nrecs is expected to be a postive integer or NULL.", call. = TRUE )
  }

  if( !( is.character(tagType) && tagType > 0) ) {
    stop( "The argument tagType is expected to be a string.", call. = TRUE )
  }

  smr2 <- smr %>% SMRMonFilterMatrix( profile = profile, type = "intersection" )

  if( is.null(smr2$M) || nrow(smr2$M) == 0 ) {
    return(NULL)
  }

  dfLabels <-
    smr2 %>%
    SMRMonRecommendByProfile( profile = profile, nrecs = nrecs, ... ) %>%
    SMRMonTakeValue

  names(dfLabels) <- gsub( smr$ItemColumnName, "Tag", names(dfLabels) )

  cbind( QueryType = "Profile", Query = paste(profile, collapse = ", "), TagType = tagType, dfLabels[1:min(nrecs,nrow(dfLabels)), c("Tag", "Score")], stringsAsFactors = FALSE )

}


##===========================================================
## Make Test By Classification
##===========================================================

#' Make a test by classification
#' @description Makes a classification test for given SMR object, tag type, and profile.
#' @param smr An SMR object.
#' @param tagType A tag type. (A string).
#' @param profile A profile. Has a form accepted by \code{\link{SMRMonClassifyByProfile}}.
#' @param nTestLabels Number of test labels in the result.
#' @param ... Additional arguments for \code{\link{SMRMonClassifyByProfile}}.
#' @return A data frame.
#' @export
MakeTestByClassification <- function( smr, tagType, profile, nTestLabels, ...) {

  if( ! SMRSparseMatrixRecommenderQ(smr) ) {
    stop( "The argument smr is expected to be a SMR object.", call. = TRUE )
  }

  ## Is this needed? SMRMonClassifyByProfile will complain.
  # if( !( is.character(tagType) && (tagType %in% smr %>% SMRMonTakeTagTypes) ) ) {
  #   stop( paste("The argument tagType is expected to be a string one of:", smr %>% SMRMonTakeTagTypes ), call. = TRUE )
  # }

  if( !( is.numeric(nTestLabels) && length(nTestLabels) == 1 && nTestLabels > 0) ) {
    stop( "The argument nTestLables is expected to be a postive integer.", call. = TRUE )
  }

  dfLabels <-
    smr %>%
    SMRMonClassifyByProfile( tagType = tagType, profile = profile, ... ) %>%
    SMRMonTakeValue

  dfLabels <-
    dfLabels %>%
    dplyr::mutate( Tag = Label )

  cbind( QueryType = "Profile", Query = paste(profile, collapse = ", "), TagType = tagType, dfLabels[1:min(nTestLabels,nrow(dfLabels)), c("Tag", "Score")], stringsAsFactors = FALSE )

}


##===========================================================
## Find same items
##===========================================================

#' Find same items
#' @description
#' For each test in specified data frame finds the labels that are "the same"
#' according to the specified SMR object and similarity threshold,
#' and makes those "same" labels to have the same (recommendation) scores.
#' @param smr An SMR object.
#' @param threshold Threshold above which two items (rows)
#' in the SMR are considered to be "the same".
#' @param itemColumnName A string specifying the column names in the result.
#' If NULL \code{smr$ItemColumnName} is used.
#' @param sampleSize A positive integer specifying a sample of items to be used.
#' If NULL all items \code{smr} are used.
#' @return A data frame.
#' @export
FindSameItems <- function( smr, threshold = 1, itemColumnName = smr$ItemColumnName, sampleSize = NULL ) {

  if( !("SMR" %in% class(smr)) ) {
    stop( "The argument smr is expected to be an SMR object.", call. = TRUE )
  }

  if( !is.numeric(threshold) && length(threshold) == 1 ) {
    stop( "The argument threshold is expected to be a number.", call. = TRUE )
  }

  if( !( is.null(itemColumnName) || is.character(itemColumnName) && length(itemColumnName) == 1 ) ) {
    stop( "The argument itemColumnName is expected to be a string or NULL.", call. = TRUE )
  }

  if( !( is.null(sampleSize) || is.numeric(sampleSize) && length(sampleSize) == 1 && sampleSize > 0 ) ) {
    stop( "The argument sampleSize is expected to be a positive number or NULL.", call. = TRUE )
  }

  if( is.null(itemColumnName) ) {
    itemColumnName <- smr$ItemColumnName
  }

  if( is.null(sampleSize) ) {
    lsItems <- rownames(smr$M)
  } else {
    lsItems <- sample( rownames(smr$M), min(nrow(smr$M), sampleSize) )
  }

  dfRes <-
    purrr::map_df( lsItems, function(itemID) {

      lsScores <- smr$M %*% t(smr$M[itemID, , drop = FALSE])

      lsPred <- Matrix::summary(lsScores)
      lsPred <- lsPred[ lsPred$x >= threshold, 1]

      if( sum(lsPred) == 0 ) {
        NULL
      } else {
        data.frame( Item = itemID, SimilarItem = rownames(smr$M)[lsPred], Score = lsScores[lsPred,1], stringsAsFactors = FALSE )
      }
    })


  dfRes <-
    dfRes %>%
    dplyr::arrange( Item, desc(Score) )

  names(dfRes) <- gsub( "Item", itemColumnName, names(dfRes) )

  dfRes
}


##===========================================================
## Make itemsets recommender
##===========================================================

#' Make item-sets recommender
#' @description Creates item-sets recommender for a given list of item-sets.
#' @param itemsets A character vector of "collapsed" frequent itemsets.
#' @return An SMR object.
#' @export
MakeItemsetsRecommender <- function( itemsets ) {

  dfRes <-
    purrr::map_df( itemsets, function(items) {

      lsProfile <- trimws( strsplit( x = items, split = ",")[[1]] )

      data.frame( Itemset = items, Tag = lsProfile, stringsAsFactors = FALSE)

    })

  matItemsets <- xtabs( formula = ~ Itemset + Tag, data = dfRes, sparse = TRUE )

  smrItemsets <- SMRMonUnit() %>% SMRMonCreateFromMatrices( matrices = c(matItemsets), tagTypes = "Tags", itemColumnName = "Itemset" )

  dfItemsetsLengths <- data.frame( Itemset = rownames(matItemsets), Length = rowSums(matItemsets), stringsAsFactors = FALSE )

  smrItemsets$ItemsetLengths <- setNames( rowSums(matItemsets), rownames(matItemsets) )

  smrItemsets
}


##===========================================================
## Represent by itemsets
##===========================================================

#' Represent by item-sets
#' @description Finds the representation a profile over the columns of
#' a given item-sets recommender.
#' @param smrItemsets An item-sets recommender.
#' @param profile A profile that has a form accepted by \code{\link{SMRMonRecommendByProfile}}.
#' @return A data frame
#' @export
RepresentByItemsets <- function( smrItemsets, profile ) {

  dfRecs <-
    smrItemsets %>%
    SMRMonRecommendByProfile( profile = profile, nrecs = NULL ) %>%
    SMRMonTakeValue()

  dfRecs <-
    dfRecs %>%
    dplyr::mutate( Length = smrItemsets$ItemsetLengths[ Itemset ] )

  dfRecs <- dfRecs %>% dplyr::filter( Score == Length )

  if( is.null(dfRecs) || nrow(dfRecs) == 0 ) {
    NULL
  } else {
    dfRecs[, c("Itemset", "Length")]
  }

}
