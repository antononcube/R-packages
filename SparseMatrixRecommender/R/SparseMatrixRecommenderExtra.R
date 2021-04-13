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
#' @import magrittr
NULL


##===========================================================
## SMRTagTypeRepresentation
##===========================================================

ListOfSparseMatrixRecommendersQ <- function(obj) {
  
  if( !is.list(obj) ) { 
    FALSE
  } else {
    res <- purrr::map_lgl( obj, function(x) { SMRSparseMatrixRecommenderQ(x) })
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
#' (Follows the naming in \code{cor}.)
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


##===========================================================
## SMRUnitizeSubMatrices
##===========================================================

#' Unitize sub-matrices
#' @description Unitizes the sub-matrices of a sparse matrix recommender.
#' @param smr A sparse matrix recommender.
#' @param tol A positive number. 
#' Matrix entries with absolute values below \code{tol} are put to 0.
#' @return A sparse matrix recommender
#' @export
SMRUnitizeSubMatrices <- function( smr, tol = 0 ) {
  
  if( !SMRSparseMatrixRecommenderQ(smr) ) {
    stop( "The argument smr is expected to be a sparse matrix recommender object.", call. = TRUE )
  }
    
  smats <- purrr::map( smr$TagTypes, function(x) SMRUnitize( smat = SMRSubMatrix(smr = smr, tagType = x), tol = tol ) )
  
  SMRCreateFromMatrices( matrices = smats, 
                         tagTypes = smr$TagTypes, 
                         itemColumnName = smr$ItemColumnName, 
                         imposeSameRowNamesQ = FALSE,
                         addTagTypesToColumnNamesQ = FALSE,
                         sep = ":")
  
}


##===========================================================
## SMRSparseMatrixSelectByRow
##===========================================================

#' Sparse matrix row-wise element selection
#' @description Selects elements row-wise using a specified criteria.
#' @param smat A sparse matrix
#' @param selectionCriteria Tag selection criteria.
#' If a positive integer for each row the number of \code{selectionCriteria} top elements are taken.
#' If a function that function is expected to give a list of booleans for a given list of numerical values.
#' The elements with values that correspond to TRUE are selected.
#' @return A sparse matrix
#' @export
SMRSparseMatrixSelectByRow <- function( smat, selectionCriteria ) {

  dfLongForm <- SMRSparseMatrixToTriplets( smat = smat )
  
  ## Tag selection
  if( is.numeric(selectionCriteria) && selectionCriteria > 0 ) {
    
    dfLongForm <- 
      dfLongForm %>%
      dplyr::group_by_at( .vars = names(dfLongForm)[1] ) %>%  
      dplyr::arrange( desc(x) ) %>% 
      dplyr::filter( dplyr::row_number() <= selectionCriteria ) %>% 
      dplyr::ungroup()
    
  } else if( is.function(selectionCriteria) ){
    
    dfLongForm <- 
      dfLongForm %>%
      dplyr::group_by_at( .vars = names(dfLongForm)[1] ) %>%  
      dplyr::filter( selectionCriteria(x) ) %>% 
      dplyr::ungroup()
    
  }
 
  xtabs( formula = x ~ i + j, data = dfLongForm, sparse = TRUE  )
}


##===========================================================
## SMRToMetadataRecommender
##===========================================================

#' Convert to a metadata recommender
#' @description Converts the recommender object into a recommender for a 
#' specified tag type.
#' @param smr A sparse matrix recommender.
#' @param tagTypeTo Tag type to make a recommender for.
#' @param nTopTags Number of top tags from \code{tagTypeTo} when making item-tag 
#' replacements.
#' @param tagTypes 	A vector tag types (strings) to make the data frame with. 
#' If NULL all tag types are used. Passed to \code{\link{SMRMatricesToLongForm}}.
#' @param tagSelectionCriteria Tag selection criteria.
#' If a positive integer for each tag type the number of \code{tagSelectionCriteria} top tags are taken.
#' If a function that function is expected to give a list of booleans for a given list of tag weights.
#' The tags with weights that correspond to TRUE are selected.
#' @param ... Additional arguments for \code{\link{SMRMatricesToLongForm}} or \code{\link{SMRCreateFromMatrices}}.
#' @return A sparse matrix recommender
#' @details The following steps are taken.
#' (1) The long form of the recommender is made.
#' (2) The items are replaced with the top tags of \code{tagTypeTo}.
#' (3) A new recommender is created with items that are the tags of \code{tagTypeTo}.
#' @export
SMRToMetadataRecommender <- function( smr, tagTypeTo, nTopTags = 1, tagTypes = NULL, tagSelectionCriteria = NULL, ... ) {
  
  if( !SMRSparseMatrixRecommenderQ(smr) ) {
    stop( "The argument smr is expected to be a sparse matrix recommender.", call. = TRUE )
  }
  
  if( !( is.character(tagTypeTo) && length(tagTypeTo) == 1 ) ) {
    stop( "The argument tagTypeTo is expected to be a string (a character vector of length one.)", call. = TRUE )
  }
  
  if( !( tagTypeTo %in% smr$TagTypes ) ) {
    stop( "The argument tagTypeTo is not a known tag type of the recommender smr.", call. = TRUE )
  }
  
  if( is.null(nTopTags) ) { nTopTags <- nrow(smr$M) }
  
  if( !( is.numeric(nTopTags) && length(nTopTags) == 1 && nTopTags > 0 ) ) {
    stop( "The argument nTopTags is expected to be a positive number or NULL.", call. = TRUE )
  }
  
  if( nTopTags > 1 ) {
    warning( "The argument nTopTags is taken to be 1: larger number of tags is not implemented (yet.)", call. = TRUE )
    nTopTags <-  1
  }
  
  if( is.null(tagTypes) ) {
    tagTypes <- setdiff(smr$TagTypes, tagTypeTo)
  }
  
  if( tagTypeTo %in% tagTypes ) {
    warning( "Removing the value of tagTypeTo from value of tagTypes.", call. = TRUE )
    tagTypes <- setdiff(tagTypes, tagTypeTo)
  }
  
  if( sum(tagTypes %in% smr$TagTypes) == 0) {
    stop( "The argument tagTypes has no known tag type of the recommender smr.", call. = TRUE )
  }
  
  if( !( is.null(tagSelectionCriteria) || is.numeric(tagSelectionCriteria) && tagSelectionCriteria > 0 || is.function(tagSelectionCriteria) ) ) {
    stop( "The argument tagSelectionCriteria is expected to be NULL, a positive integer, or a function.", call. = TRUE )
  }
  
  ## Assign default values for ...
  lsAllArgs <- c( as.list(environment()), list(...) )
  lsDefaults <- list( removeTagTypePrefixesQ = TRUE, sep = ":", imposeSameRowNamesQ = TRUE, addTagTypesToColumnNamesQ = NULL )
  
  lsAllArgs <- 
    Reduce( 
      function(a,x) if( x %in% names(a) ) { a } else { c( a, lsDefaults[x] ) }, 
      init = lsAllArgs, 
      x = names(lsDefaults) 
    )

  ## To long form
  dfVectorTypesLongForm <- 
    SMRMatricesToLongForm( 
      smr = smr, 
      tagTypes = unique( c(tagTypeTo, tagTypes) ), 
      removeTagTypePrefixesQ = lsAllArgs[["removeTagTypePrefixesQ"]], 
      sep = lsAllArgs[["sep"]]  
    )
  
  
  ## Make mapping rules
  dfIDToTag <- 
    dfVectorTypesLongForm %>% 
    dplyr::filter( TagType == tagTypeTo & Weight > 0 ) %>% 
    dplyr::group_by_at( .vars = c( smr$ItemColumnName ) ) %>% 
    dplyr::arrange(dplyr::desc(Weight)) %>% 
    dplyr::filter( dplyr::row_number() <= nTopTags ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate( NewID = Value ) %>% 
    dplyr::select_at( .vars = c(smr$ItemColumnName, "NewID") )
  
  
  lsIDToTag <- setNames( dfIDToTag$NewID, dfIDToTag[[smr$ItemColumnName]] )
  
  ## Remove tagTypeTo records from long form
  dfVectorTypesLongForm <- 
    dfVectorTypesLongForm %>% 
    dplyr::filter( TagType != tagTypeTo )
  

  if( is.null(dfVectorTypesLongForm) || nrow(dfVectorTypesLongForm) == 0 ) {
    stop( "Empty long form was obtained.", call. = TRUE )
  }
  
  ## Transform the long form with mapping
  if( nTopTags == 1 ) {
    ## Map items to tagTypeTo values
    
    dfVectorTypesLongForm[[ smr$ItemColumnName ]] <- lsIDToTag[ dfVectorTypesLongForm[[ smr$ItemColumnName ]]  ]
    
    dfVectorTypesLongForm <- dfVectorTypesLongForm[ !is.na(dfVectorTypesLongForm[[ smr$ItemColumnName ]]), ]
    
  } else {
    ## Using inner join for the new IDs (that are tagTypeTo values)

    dfVectorTypesLongForm <- 
      dfVectorTypesLongForm %>% 
      dplyr::inner_join( dfIDToTag, by = smr$ItemColumnName )
    
    dfVectorTypesLongForm[[ smr$ItemColumnName ]] <- dfVectorTypesLongForm[[ "NewID" ]]
    
    dfVectorTypesLongForm <- 
      dfVectorTypesLongForm %>% 
      dplyr::mutate( NewID = NULL )
  }

  ## Create contingency matrices from the transformed long form
  smats <- 
    purrr::map( split(dfVectorTypesLongForm, dfVectorTypesLongForm$TagType ), function(x) { 
      mat <- xtabs( formula = as.formula( paste( "Weight ~ ", smr$ItemColumnName, " + Value" ) ), data = x, sparse = TRUE )
    } )
  
  ## Select tags
  if( !is.null(tagSelectionCriteria) ) {
    
    smats <- 
      purrr::map( smats, function(x) { 
        SMRSparseMatrixSelectByRow( smat = x, selectionCriteria = tagSelectionCriteria )
      } )
    
  }
  
  ## Create recommender
  SMRCreateFromMatrices( 
    matrices = smats, 
    itemColumnName = tagTypeTo, 
    imposeSameRowNamesQ = lsAllArgs[["imposeSameRowNamesQ"]], 
    addTagTypesToColumnNamesQ = lsAllArgs[["addTagTypesToColumnNamesQ"]] 
  )
} 
