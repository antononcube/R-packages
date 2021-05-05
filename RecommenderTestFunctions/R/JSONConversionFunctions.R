

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
## GetRandomIdentifiers
##===========================================================

#' Get random identifiers
#' @description Makes a character vector with random unique string of specified length.
#' @param n Number of identifiers to be generated.
#' @param length Number of characters per identifier.
#' @param pattern Character vector specifying character classes to draw elements from;
#' see \code{\link{stringi-search-charclass}}.
#' @param prefix A prefix string.
#' @return A character vector
#' @details Uses internally \code{\link{stringi::stri_rand_strings}}.
#' @export
GetRandomIdentifiers <- function( n, length = 12, pattern = "[A-Za-z0-9]",  prefix = "" ) {

  if( ! ( is.numeric(n) && n > 0 )) {
    stop( "The argument n is expected to be a positive integer.", call. = TRUE )
  }

  if( ! ( is.numeric(length) && length > 0 )) {
    stop( "The argument length is expected to be a positive integer.", call. = TRUE )
  }

  if( is.null(prefix) ) { prefix = "" }
  if( ! is.character(prefix) ) {
    stop( "The argument prefix is expected to be a string.", call. = TRUE )
  }

  res <- stringi::stri_rand_strings(n = n, length = length, pattern = pattern )
  res <- unique(res)

  while( length(res) < n ) {
    res <- c( res, stringi::stri_rand_strings( n = floor(n/2), length = length, pattern = pattern ) )
    res <- unique(res)
  }

  res <- res[1:n]

  if( nchar(prefix) > 0 ) {
    res <- paste0( prefix, res)
  }

  res
}



##===========================================================
## To JSON tests
##===========================================================

#' Convert data frame to JSON test specifications
#' @description Converts a given data frame to JSON test specifications.
#' @param dfTests A data frame with tests.
#' @param minIntersectionCount Number for the expected min intersection counts per test.
#' @param minCorrelation Number for the expected min correlation per test.
#' @param correlationMethod String for the correlation method.
#' It is expected to be one if "Pearson", "Spearman", or "Kendall",
#' but it is not enforced.
#' @param testType String for the test type.
#' It is expected to be one "ComparisonTest" or "ConditionTest",
#' but that is not enforced.
#' @param recommenderType String for the recommender type.
#' It is expected to be one of "MetadataRecommender" or "ItemRecommender",
#' but it is not enforced.
#' @param testIdentifierColumns A character vector with the column names that
#' the unique values of which correspond to unique tests.
#' @param collapseQ Should the JSON test specifications be joined into one JSON object or not?
#' If \code{TRUE} using \code( paste("{", paste(res, collapse= ", "), "}")}.
#' @return A JSON character vector
#' @details
#' @export
ToJSONTests <- function( dfTests,  minIntersectionCount,
                         minCorrelation = 0.7, correlationMethod = "Pearson",
                         testType = "Comparison", recommenderType = "ItemRecommender",
                         testIdentifierColumns = c("Domain", "QueryTagType", "Query"),
                         collapseQ = FALSE ) {

  if( ! is.data.frame(dfTests) ) {
    stop( "The argument dfTests is expected to be a data frame.", call. = TRUE )
  }

  lsExpectedColumnNames <- testIdentifierColumns
  if( ! ( mean( lsExpectedColumnNames %in% colnames(dfTests) ) == 1 ) ) {
    stop( paste( "The argument dfTests is expected to be a data frame that has the columns namess:", paste(lsExpectedColumnNames, collapse = ",") ), call. = TRUE )
  }


  if( ! ( is.numeric(minIntersectionCount) && minIntersectionCount > 0 )) {
    stop( "The argument minIntersectionCount is expected to be a positive integer.", call. = TRUE )
  }

  if( ! ( is.numeric(minCorrelation) &&  -1 <= minCorrelation && minCorrelation <= 1 )) {
    stop( "The argument minCorrelation is expected to be a number between -1 and 1.", call. = TRUE )
  }

  if( ! is.character(correlationMethod) ) {
    stop( "The argument correlationMethod is expected to be a string.", call. = TRUE )
  }

  if( ! is.character(testType) ) {
    stop( "The argument testType is expected to be a string.", call. = TRUE )
  }

  if( ! is.character(recommenderType) ) {
    stop( "The argument recommenderType is expected to be a string.", call. = TRUE )
  }

  lsIDs <- GetRandomIdentifiers(nrow(unique(dfTests[, testIdentifierColumns])))

  lsCode <-
    purrr::map2( split(dfTests, dfTests[, testIdentifierColumns]), lsIDs, function(dfX, id) {
        list(
          TestID = jsonlite::unbox(id),
          RecommenderType = jsonlite::unbox(recommenderType),
          TestType = jsonlite::unbox(testType),
          MinIntersectionCount = jsonlite::unbox(minIntersectionCount),
          CorrelationMethod = jsonlite::unbox(correlationMethod),
          MinCorrelation = jsonlite::unbox(minCorrelation),
          DataTable = dfX
        )
    })
  lsCode <- setNames(lsCode, lsIDs)


  if( collapseQ ) {

    lsCode <- jsonlite::toJSON( lsCode, dataframe = "columns", pretty = TRUE)

  } else {

    lsCode <-
      purrr::map( lsCode, function(trec) {
        jsonlite::toJSON( trec, dataframe = "columns", pretty = TRUE)
      })

  }

  lsCode
}
