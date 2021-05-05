

#' @import SparseMatrixRecommender
#' @import SMRMon
#' @import Matrix
#' @import magrittr
#' @import purrr
#' @import dplyr
NULL


##===========================================================
## SummarizeRanks
##===========================================================

#' Summarize ranks
#' @description Summarizes the ranks of given data frame.
#' @param data A data frame.
#' @param focusTags A character vector with tags over which the summaries are computed.
#' @param summariseByTagTypeQ Should the tag-summaries be further summarized by tag type or not?
#' @param A data frame.
#' @export
SummarizeRanks <- function( data, focusTags = NULL, summariseByTagTypeQ = FALSE  ) {

  expectedColumnNames <- c( "Profile", "TagType", "Score", "Tag" )

  if( !( is.data.frame(data) && sum( expectedColumnNames %in% colnames(data) ) == length(expectedColumnNames) ) ) {
    stop( paste0( "The argument data is expected to be a data frame that has the columns: '", paste(expectedColumnNames, collapse = "', '"), "'."), call. = TRUE )
  }

  if( !( is.null(focusTags) || is.character(focusTags) ) ) {
    stop( "The argument focusTags is expected to be a character vector or NULL.", call. = TRUE )
  }

  if( !( is.null(summariseByTagTypeQ) || is.logical(summariseByTagTypeQ) && length(summariseByTagTypeQ) == 1 ) ) {
    stop( "The argument summariseByTagTypeQ is expected to be a logical value or NULL.", call. = TRUE )
  }


  dfBaselineQuery <-
    data %>%
    dplyr::group_by( TagType, Profile ) %>%
    dplyr::arrange(desc(Score)) %>%
    dplyr::mutate( Rank = row_number() ) %>%
    dplyr::ungroup()

  if( is.character(focusTags) ) {

    dfBaselineQuery <-
      dfBaselineQuery %>%
      dplyr::filter( Tag %in% focusTags )

  }

  dfBaselineQuery <-
    dfBaselineQuery %>%
    dplyr::group_by( TagType, Tag ) %>%
    dplyr::summarise_at( .vars = "Rank", .funs = c( Min = min, Max = max, Mean = mean, Median = median ) )

  if( is.null(summariseByTagTypeQ) || !summariseByTagTypeQ ) {

    dfBaselineQuery

  } else {

    dfBaselineQuery %>%
      dplyr::group_by( TagType ) %>%
      dplyr::summarise_at( .vars = c("Min", "Max", "Mean", "Median"), .funs = mean )

  }

}


##===========================================================
## TestResultsComparisonMetrics
##===========================================================

#' Test results comparison metrics
#' @description Compares two recommendations results data frames.
#' Intersection size and correlation of scores are computed.
#' @param result1 First recommendations set.
#' @param result2 Second recommendations set.
#' @param nTop The top number of recommendations to take.
#' @param by A character vector with column names to group by.
#' If NULL no grouping is made.
#' @param tagColumnName The column that has the recommendation tags.
#' @param corMethod Method for \code{\link{cor}}.
#' (One of \code{c("pearson", "kendall", "spearman")}.)
#' @return A data frame.
#' @export
TestResultsComparisonMetrics <- function( result1, result2, nTop = 10, by = c("QueryType", "Query", "TagType"), tagColumnName = "Tag", corMethod = "pearson" ) {

  expectedColumnNames <- c( by, tagColumnName, c( "Score" ) )

  if( !( is.data.frame(result1) && sum( expectedColumnNames %in% colnames(result1) ) == length(expectedColumnNames) ) ) {
    stop( paste0( "The argument result1 is expected to be a data frame that has the columns: '", paste(expectedColumnNames, collapse = "', '"), "'."), call. = TRUE )
  }

  if( !( is.data.frame(result2) && sum( expectedColumnNames %in% colnames(result2) ) == length(expectedColumnNames) ) ) {
    stop( paste0( "The argument result2 is expected to be a data frame that has the columns: '", paste(expectedColumnNames, collapse = "', '"), "'."), call. = TRUE )
  }

  # Joined results

  dfJoinedResults <- result1

  if( is.character(by) ) {
    dfJoinedResults <-
      dfJoinedResults %>%
      dplyr::group_by_at( .vars = by )
  }

  dfJoinedResults <-
    dfJoinedResults %>%
    dplyr::arrange( dplyr::desc(Score) ) %>%
    dplyr::filter( dplyr::row_number() <= nTop ) %>%
    dplyr::mutate( TotalTagsCount = n() ) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join( result2, by = c( by, tagColumnName ) )


  if( is.null(dfJoinedResults) || nrow(dfJoinedResults) == 0 ) {
    return(NULL)
  }

 # Intersection and correlation statistics

  dfBatchComparison <- dfJoinedResults

  if( is.character(by) ) {
    dfBatchComparison <-
      dfBatchComparison %>%
      dplyr::group_by_at( .vars = by )
  }

  assertthat::assert_that( mean( c("Score.x", "Score.y") %in% colnames(dfBatchComparison) ) == 1 )

  dfBatchComparison <-
    dfBatchComparison %>%
    dplyr::summarise( TotalTagsCount = TotalTagsCount[[1]],
                      CommonTagsCount = n(),
                      Cor = ifelse( n() < 3, NA, ifelse( sd(Score.x) == 0 && sd(Score.y) == 0, 1, cor( x = Score.x, y = Score.y, method = corMethod ) ) ), .groups = "drop" ) %>%
    dplyr::ungroup()

  dfBatchComparison
}

