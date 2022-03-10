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
#' @import feather
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
#' @param tagTypes A character vector with tag types to unitize.
#' If NULL all tag types of \code{smr} are unitized.
#' @param tol A positive number. 
#' Matrix entries with absolute values below \code{tol} are put to 0.
#' @return A sparse matrix recommender
#' @export
SMRUnitizeSubMatrices <- function( smr, tagTypes = NULL, tol = 0 ) {
  
  if( !SMRSparseMatrixRecommenderQ(smr) ) {
    stop( "The argument smr is expected to be a sparse matrix recommender object.", call. = TRUE )
  }
    
  if( is.null(tagTypes) ) { tagTypes <- smr$TagTypes }
  
  if( !is.character(tagTypes) ) {
    stop( "The argument tagTypes is expected to be a vector with known tag types or NULL.", call. = TRUE )
  }
  
  lenTagTypes <- length(tagTypes)
  
  tagTypes <- intersect( x = tagTypes, y = smr$TagTypes )

  if( length(tagTypes) == 0 ) {
    stop( "None of the elements of the argument tagTypes are known tag types.", call. = TRUE )
  } else if ( length(tagTypes) < lenTagTypes ) {
    warning( "Not all elements of the argument tagTypes are known tag types.", call. = TRUE )
  }
  
  smats <- purrr::map( tagTypes, function(x) SMRUnitize( smat = SMRSubMatrix(smr = smr, tagType = x), tol = tol ) )
  names(smats) <- tagTypes
  
  if( length(tagTypes) < length(smr$TagTypes) ) {
    
    tagTypes2 <- setdiff( smr$TagTypes, tagTypes)
    
    smats2 <- purrr::map( tagTypes2, function(x) SMRSubMatrix(smr = smr, tagType = x) )
    names(smats2) <- tagTypes2
    
    smats <- c( smats, smats2 )
  } 
  
  SMRCreateFromMatrices( matrices = smats[smr$TagTypes], 
                         tagTypes = NULL,
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
## SMRToMetadataRecommenderByReplacement
##===========================================================

#' Convert to a metadata recommender by replacement
#' @description Converts the recommender object into a recommender for a 
#' specified tag type using replacements in the long form representation of the
#' recommender matrix.
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
SMRToMetadataRecommenderByReplacement <- function( smr, tagTypeTo, nTopTags = 1, tagTypes = NULL, tagSelectionCriteria = NULL, ...) {
  
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
    addTagTypesToColumnNamesQ = lsAllArgs[["addTagTypesToColumnNamesQ"]],
    sep = lsAllArgs[["sep"]]
  )
} 


##===========================================================
## SMRToMetadataRecommender (by matrix product)
##===========================================================

#' Convert to a metadata recommender
#' @description Converts the recommender object into a recommender for a 
#' specified tag type using replacements in the long form representation of the
#' recommender matrix.
#' @param smr A sparse matrix recommender.
#' @param tagTypeTo Tag type to make a recommender for.
#' @param tagTypes 	A vector tag types (strings) to make the data frame with. 
#' If NULL all tag types are used. Passed to \code{\link{SMRMatricesToLongForm}}.
#' @param tagTypeMatrix A sparse matrix of item IDs vs tags of \code{tagTypeTo}.
#' If NULL then \code{SMRSubMatrix(smr, tagTypeTo)} is used. 
#' @param normalizerFunc An LSI normalizer function.
#' One of NULL, "None", "Cosine", "Sum", or "Max".
#' If NULL then it is same as "None". 
#' See \code{SMRApplyTermWeightFunctions}.
#' @return A sparse matrix recommender
#' @details The following steps are taken.
#' (1) If \code{tagTypeMatrix} is NULL then \code{tagTypeMatrix <- SMRSubMatrix(smr, tagTypeTo)}.
#' (2) Normalize the columns of \code{tagTypeMatrix} using \code{normalizerFunc}.
#' (3) Each recommender sub-matrix \code{m} is multiplied by \code{tagTypeMatrix}, i.e. \code{t(tagTypeMatrix) %*% m}.
#' (3) A new recommender is created with items that are the tags of \code{tagTypeTo}.
#' @export
SMRToMetadataRecommender <- function( smr, tagTypeTo, tagTypes = NULL, tagTypeMatrix = NULL, normalizerFunc = NULL ) {
  
  if( !SMRSparseMatrixRecommenderQ(smr) ) {
    stop( "The argument smr is expected to be a sparse matrix recommender.", call. = TRUE )
  }
  
  ## Process tagTypeTo
  if( !( is.character(tagTypeTo) && length(tagTypeTo) == 1 ) ) {
    stop( "The argument tagTypeTo is expected to be a string (a character vector of length one.)", call. = TRUE )
  }
  
  if( !( tagTypeTo %in% smr$TagTypes ) ) {
    stop( "The argument tagTypeTo is not a known tag type of the recommender smr.", call. = TRUE )
  }
  
  ## Process tagTypes
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
  
  ## Process tagTypeMatrix
  if( !( is.null(tagTypeMatrix) || SMRSparseMatrixQ(tagTypeMatrix) ) ) {
    stop( "The argument tagTypeMatrix is expected to be NULL or a sparse matrix.", call. = TRUE )
  }
  
  if( is.null(tagTypeMatrix) ) {
    tagTypeMatrix <- SMRSubMatrix( smr, tagTypeTo )
  }
  
  if( ! ( mean( rownames(tagTypeMatrix) == rownames(smr$M) ) == 1 ) ) {
    stop( "The argument tagTypeMatrix has rownames that are different than the rownames of smr$M.", call. = TRUE )
  }
  
  ## Process normalizerFunc
  if( !( is.null(normalizerFunc) || is.character(normalizerFunc) && length(normalizerFunc) == 1 ) ) {
    stop( "The argument normalizerFunc is expected to be NULL or a string (a character vector of length one.)", call. = TRUE )
  }
  
  ## Obtain tagTypeMatrix
  tagTypeMatrix <- t(tagTypeMatrix)
  
  ## Normalize the columns of tagTypeMatrix
  if( !( is.null(normalizerFunc) || normalizerFunc == "None") ) {
    tagTypeMatrix <- 
      SMRApplyTermWeightFunctions( 
        docTermMat = tagTypeMatrix, 
        globalWeightFunction = "None",
        localWeightFunction = "None",
        normalizerFunction = normalizerFunc )
  }
  
  ## Get the recommender contingency matrices 
  smats <- 
    purrr::map( tagTypes, function(tt) { 
      SMRSubMatrix(smr, tt)
    } )
  names(smats) <- tagTypes
  

  ## Multiply sub-matrices
  smats <- 
      purrr::map( smats, function(m) { 
        tagTypeMatrix %*% m
      } )
  

  ## Create recommender
  SMRCreateFromMatrices( 
    matrices = smats, 
    itemColumnName = tagTypeTo, 
    imposeSameRowNamesQ = TRUE
  )
} 


##===========================================================
## SMRCrossTabulateTagTypes
##===========================================================

#' Cross tabulate tag types
#' @description Cross tabulates two specified tag types.
#' @param smr An SMR object.
#' @param tagType1 Tag type corresponding to the rows of the cross tabulation.
#' @param tagType2 Tag type corresponding to the common of the cross tabulation.
#' @param value Which value to use in the cross tabulation formula?
#' One of \code{c("none", "left", "right", "multiply")}.
#' If "none" then \code{xtabs( ~ tagType1 + tagType2, ...)} is used.
#' @param normalizerFunction A string specifying the normalization function 
#' for the result sparse matrix. 
#' If NULL then no normalization is done.
#' @details Uses \code{\link{xtabs}} and \code{\link{SMRApplyTermWeightFunctions}}.
#' @return A sparse matrix
#' @export
SMRCrossTabulateTagTypes <- function( smr, tagType1, tagType2, value = "none", normalizerFunction = NULL ) {
  
  expectedValueValues = c( "none", "left", "right", "multiply" )
  if( ! ( value %in% expectedValueValues )  ) {
    stop( paste0( "The expected value of the argument value is one of: '", paste0(expectedValueValues, collapse = "', '"), "'."  ), call. = TRUE )
  }
  
  ## Join
  dfQuery <- 
    SMRSparseMatrixToDataFrame( smr, tagType = tagType1 ) %>% 
    dplyr::inner_join( SMRSparseMatrixToDataFrame( smr, tagType = tagType2 ), by = smr$ItemColumnName )
  
  ## Formula
  if( value == "none" ) {
    
    frm <- as.formula( paste0( " ~ ", tagType1, " + ", tagType2 ) )
    
  } else if( value == "left" ) {
    
    frm <- as.formula( paste0( "Weight.x ~ ", tagType1, " + ", tagType2 ) )
    
  } else if( value == "right" ) {
    
    frm <- as.formula( paste0( "Weight.y ~ ", tagType1, " + ", tagType2 ) )
    
  } else {
    ## value == "multiply"
    dfQuery <- 
      dfQuery %>% 
      dplyr::mutate( Weight = Weight.x * Weight.y )
    
    frm <- as.formula( paste0( "Weight ~ ", tagType1, " + ", tagType2 ) )
  }
  
  ## Cross tabulate
  matProfiles <- xtabs( frm, dfQuery, sparse = TRUE ) 
  
  ## Normalize
  if( !is.null(normalizerFunction) ) {
    matProfiles <- SMRApplyTermWeightFunctions( matProfiles, globalWeightFunction = "None", localWeightFunction = "None", normalizerFunction = normalizerFunction )
  }
  
  matProfiles
}


##===========================================================
## SMRExportToFeatherFile
##===========================================================

#' Export to feather format file
#' @description Exports an SMR object to a feather format file.
#' @param smr An SMR object.
#' @param path Path to feather file.
#' @details Uses \code{\link{SMRMatricesToLongForm}} and \code{\link{feather::write_feather}}.
#' @return NULL
#' @export
SMRExportToFeatherFile <- function( smr, path ) {
  
  feather::write_feather( x = SMRMatricesToLongForm(smr = smr), path = path )
  
}
  

##===========================================================
## SMRImportFromFeatherFile
##===========================================================

#' Import from feather format file
#' @description Exports an SMR object to a feather format file.
#' @param path Path to feather file.
#' @details Uses \code{\link{SMRCreateFromLongForm}} and \code{\link{feather::read_feather}}.
#' @return An SMR object
#' @export
SMRImportFromFeatherFile <- function( path ) {
  
  data <- feather::read_feather( path = path )
  
  SMRCreateFromLongForm( data = data, itemColumnName = colnames(data)[[1]] )
  
}