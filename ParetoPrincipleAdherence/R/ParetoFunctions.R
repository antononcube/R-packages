##=======================================================================================
## Pareto Principle adherence functions in R
## Copyright (C) 2015-2017, 2019  Anton Antonov
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
##
## This R script has function definitions for Pareto Principle adherence verification
## and analysis.
## Instead of "Pareto Principle" the phrase "Pareto law" is used.
##
## TODO: Better function names.
##
## Date started: April, 2014
## Updated: February 2015, March, 2015, January 2016, February 2019
##=======================================================================================

library(dplyr)
library(purrr)
library(ggplot2)

##===========================================================
## Utility functions
##===========================================================

MakeParetoData <- function( data ) {

  if( !( is.data.frame(data) || is.character(data) || is.factor(data) || is.numeric(data) ) ) {
    stop( "The argument data is expected to be a data frame, a character, factor, or numerical vector.", call. = TRUE )
  }

  if( is.data.frame(data) && sum( c("Item", "Weight") %in% names(data) ) < 2 ) {
    stop( "When the argument data is a data frame it is expected to have the columns \"Item\" and \"Weight\".", call. = TRUE )
  }

  if( is.factor(data) ) {
    data <- as.character(data)
  }

  if( is.character(data) ) {
    data <- data.frame( Item = data, Weight = 1, stringsAsFactors = FALSE )
  }

  if( is.numeric(data) && is.null(names(data)) ) {
    data <- setNames(data, 1:length(data) )
  }

  if( is.numeric(data) ) {
    data <- data.frame( Item = names(data), Weight = data )
  }

  data
}


##===========================================================
## Pareto statistic functions
##===========================================================

#' Pareto statistic for numerical vector
#' @param data A numeric vector.
#' @param normalizeQ Should the Pareto statistic be normalized with the total or not?
#' @return A numeric vector.
#' @details The simple formula
#' \code{cumsum(rev(sort(data))) / sum(data)}
#' is used.
#' @export
ParetoForNumericalVector <- function( data, normalizeQ = TRUE ) {

  if( !is.numeric(data) ) {
    stop( "The argument data is expected to be a numeric vector.", call. = TRUE )
  }

  if( sum(is.na(data)) > 0 ) {
    warning( "NA's are removed", call. = TRUE )
    data <- data[ !is.na(data) ]
  }

  if( length(data) == 0 ) { return(0) }

  res <- cumsum(rev(sort(data)))

  if( normalizeQ ) {
    res <- res / res[[length(res)]]
  }

  res
}

#' Pareto statistic for categorical data.
#' @description A synonym/shortcut for \code{\link{ParetoForWeightedItems}}.
#' @param data A character or factor vector.
#' @param normalizeQ Should the Pareto statistic be normalized with the total or not?
#' @return A data frame with columns c( "Item", "ParetoFraction" ).
#' @export
ParetoForCategoricalVector <- function( data, normalizeQ = TRUE ) {

  if( is.factor(data) ) {
    data <- as.character(data)
  }

  if( !is.character(data) ) {
    stop( "The argument data is expected to be a character vector.", call. = TRUE )
  }

  ParetoForWeightedItems( data )
}

#' Pareto statistic for weighted items.
#' @description The Pareto fractions are given for each weighted item.
#' The items are sorted in descending order according to their weight.
#' The Pareto fractions are derived as ratios between the cumulative sum of the weights
#' and the total weight.
#' @param data A data frame with columns "Item" and "Weight", a numerical vector,
#' or a categorical vector.
#' @param normalizeQ Should the Pareto statistic be normalized with the total or not?
#' @return A data frame with columns c( "Item", "ParetoFraction" ).
#' @details If \code{data} is a character vector then the weights of the elements are assumed 1.
#' If \code{data} is a numerical vector with named elements that vector is turned into
#' item-weight data frame.
#' If \code{data} is a numerical vector without named elements that vector is turned into
#' item-weight data frame with the items being vector's indexes.
#' @export
ParetoForWeightedItems <- function( data, normalizeQ = TRUE ) {

  data <- MakeParetoData( data )

  ## Aggregation in case we have duplicated names
  data <-
    data %>%
    dplyr::group_by( Item ) %>%
    dplyr::summarise( Weight = sum(Weight) ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(Weight))

  pfs <- cumsum(data$Weight)

  if( normalizeQ ) {
    pfs <- pfs / max(pfs)
  }

  data.frame( Item = data$Item, ParetoFraction = pfs, stringsAsFactors = FALSE )
}


#' Pareto for variables.
#' @description Computes the Pareto statistic for each of the specified columns.
#' @param data A data frame.
#' @param columnNames The column names of the variables.
#' @param normalizeQ Should the Pareto statistic be normalized with the total or not?
#' @return A data frame with columns c( "Variable", "Item", "ParetoFraction" ).
#' @details This function works on the wide form the data.
#' The function \code{\link{ParetoForWeightedItems}} works on the long form.
#' Note that specified columns can be a mix of categorical and numerical.
#' @export
ParetoForVariables <- function( data, columnNames, normalizeQ = TRUE ) {

  if( sum(columnNames %in% colnames(data)) < length(columnNames) ) {
    warning( "Some of the specified column names are unknown.", call. = TRUE )
    columnNames <- columnNames[ columnNames %in% colnames(data) ]
  }

  if( length(columnNames) == 0 ) {
    stop( "The argument columnNames is expected to be a non-empty character vector with column names of the argument data.", call. = TRUE )
  }

  purrr::map_dfr( columnNames, function(x) {
    cbind( Variable = x, ParetoForWeightedItems( data = data[,x], normalizeQ = normalizeQ ), stringsAsFactors = FALSE )
  })

}



##===========================================================
## Plot functions
##===========================================================

#' Pareto plot for a vector.
#' @param data A character, factor, or numerical vector.
#' @export
ParetoPlot <- function( data ) {

  if( !( is.character(data) || is.factor(data) || is.numeric(data) ) ) {
    stop( "The argument data is expected to be a character, factor, or numerical vector.", call. = TRUE )
  }

  ParetoPlotForVariables( data.frame(Variable = paste(class(data), collapse = " "), Value = data) )
}


#' Pareto plot for variables.
#' @description Make a plot that demonstrates the adherence to the Pareto law of a
#' a data frame with item names and weight values.
#' @param data A two column data frame with the first column of a variable name
#' and second column a numerical vector \code{c("Variable", "Value")}.
#' @param normalizeQ Should the Pareto cumulative values be normalized or not?
#' @param separatedPlotsQ Should the plotted Pareto curves be separated or not?
#' @param main A string for the title of the plot.
#' @param scales Sames as \code{scales} of \code{ggplot2::facet_wrap}.
#' @param nrow Sames as \code{nrow} of \code{\link{ggplot2::facet_wrap}}.
#' @param ncol Sames as \code{ncol} of \code{\link{ggplot2::facet_wrap}}.
#' @param ... Parameters for \code{\link{ggplot2::theme}}
#' @details
#' The data frame \code{data} is split by \code{data$Variable}.
#' The column \code{data$Value} is used to make the Pareto Principle curves.
#' Each plot has percentage vertical lines.
#' @details Pareto plots
#' @export
ParetoPlotForVariables <- function( data, normalizeQ = TRUE, separatedPlotsQ = TRUE,
                                    main = NULL, scales = "fixed", nrow = NULL, ncol = NULL, ...  ) {

  if( !is.data.frame(data) || sum( c("Variable", "Value") %in% colnames(data) ) < 2 ) {
    stop( "The argument data is expected to be a data frame with columns \"Variable\" and \"Value\".")
  }

  qdf <-
    purrr::map_df( split( data, data[["Variable"]] ), function(x) {

      if( nrow(x) == 0 ) {

        NULL

      } else {

        pdf <- ParetoForWeightedItems( data = x$Value, normalizeQ = normalizeQ )

        pres <- data.frame( Variable = x$Variable[[1]],
                            Item = as.character(pdf$Item),
                            Index = 1:nrow(pdf),
                            ParetoFraction = pdf$ParetoFraction,
                            stringsAsFactors = FALSE )

        cbind( pres,
             p10 = 0.1*nrow(pres), p20 = 0.2*nrow(pres), p30 = 0.3*nrow(pres),  p40 = 0.4*nrow(pres),  p50 = 0.5*nrow(pres) )
      }

    })


  if( separatedPlotsQ ) {

    ggplot2::ggplot(qdf) +
      ggplot2::geom_line(  ggplot2::aes( x = Index, y = ParetoFraction ) ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p10), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p20), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p30), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p40), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p50), linetype = 3 ) +
      ggplot2::facet_wrap( ~ Variable, scales = scales, nrow = nrow, ncol = ncol ) +
      ggplot2::theme( ... )

  } else {

    ggplot2::ggplot(qdf) +
      ggplot2::geom_line(  ggplot2::aes( x = Index, y = ParetoFraction, color = Variable ) ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p10), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p20), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p30), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p40), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p50), linetype = 3 ) +
      ggplot2::theme( ... )

  }
}


#' Pareto plots over the columns of a data frame.
#' @description Apply \code{\link{ParetoLawData}} function over a list of names
#' and make a multi-panel ggplot.
#' @param data A data frame.
#' @param columnNames A list of column names corresponding to categorical columns in \code{data}.
#' @param normalizeQ Should the Pareto cumulative values be normalized or not?
#' @param separatedPlotsQ Should the plotted Pareto curves be separated or not?
#' @param main A string for the title of the plot.
#' @param scales Sames as \code{scales} of \code{ggplot2::facet_wrap}.
#' @param nrow Sames as \code{nrow} of \code{\link{ggplot2::facet_wrap}}.
#' @param ncol Sames as \code{ncol} of \code{\link{ggplot2::facet_wrap}}.
#' @param ... Parameters for \code{\link{ggplot2::theme}}
#' @details This function make the Pareto plots over a wide form data frame.
#' The function \code{\link{ParetoPlotForVariables}} makes the plots over long form data.
#' @details Pareto plots
#' @export
ParetoPlotForColumns <- function( data, columnNames = colnames(data),
                                  normalizeQ = TRUE, separatedPlotsQ = TRUE,
                                  main = NULL, scales = "fixed", nrow = NULL, ncol = NULL, ... ) {

  if( sum(columnNames %in% colnames(data)) < length(columnNames) ) {
    warning( "Some of the specified column names are unknown.", call. = TRUE )
    columnNames <- columnNames[ columnNames %in% colnames(data) ]
  }

  if( length(columnNames) == 0 ) {
    stop( "The argument columnNames is expected to be a non-empty character vector with column names of the argument data.", call. = TRUE )
  }

  ## Very similar to the code above, but I did not figure out a good refactoring.
  qdf <-
    purrr::map_df( columnNames, function(x) {

      pdf <- ParetoForWeightedItems( data = data[[x]], normalizeQ = normalizeQ )

      pres <- data.frame( Variable = x,
                          Item = as.character(pdf$Item),
                          Index = 1:nrow(pdf),
                          ParetoFraction = pdf$ParetoFraction,
                          stringsAsFactors = FALSE )

      cbind( pres,
             p10 = 0.1*nrow(pres), p20 = 0.2*nrow(pres), p30 = 0.3*nrow(pres),  p40 = 0.4*nrow(pres),  p50 = 0.5*nrow(pres) )

    })


  if( separatedPlotsQ ) {

    ggplot2::ggplot(qdf) +
      ggplot2::geom_line(  ggplot2::aes( x = Index, y = ParetoFraction ) ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p10), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p20), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p30), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p40), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p50), linetype = 3 ) +
      ggplot2::facet_wrap( ~ Variable, scales = scales, nrow = nrow, ncol = ncol ) +
      ggplot2::theme( ... )

  } else {

    ggplot2::ggplot(qdf) +
      ggplot2::geom_line(  ggplot2::aes( x = Index, y = ParetoFraction, color = Variable ) ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p10), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p20), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p30), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p40), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p50), linetype = 3 ) +
      ggplot2::theme( ... )

  }
}
