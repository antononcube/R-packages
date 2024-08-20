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
## ʇǝu˙oǝʇsod@ǝqnɔuouoʇuɐ,
## Windermere, Florida, USA.
##
##=======================================================================================
##
## This R package has function definitions for Pareto Principle adherence verification
## and analysis.
##
## The phrase "Pareto statistic" for a vector V means the formula:
##
##   cumsum(rev(sort(V))) / sum(V) .
##
## The initial version of this package was based on the source code file:
##
##   https://github.com/antononcube/MathematicaForPrediction/blob/master/R/ParetoLawFunctions.R
##
## The current version of the package code is a completely revised system of function signatures.
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
#' @return A data frame with columns \code{c("Item", "ParetoFraction")}.
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
#' @description Compute the Pareto position and fraction for each weighted item.
#' The items are sorted in descending order according to their weight.
#' (That gives the Pareto positions.)
#' The Pareto fractions are derived as ratios between the cumulative sum of the weights
#' and the total weight.
#' @param data A data frame with columns "Item" and "Weight", a numerical vector,
#' or a categorical vector.
#' @param normalizeQ Should the Pareto statistic be normalized with the total or not?
#' @return A data frame with columns \code{c("Item", "ParetoFraction")}.
#' @details If \code{data} is a character vector then the weights of the elements are assumed 1.
#' If \code{data} is a numerical vector with named elements that vector is turned into
#' a item-weight data frame.
#' If \code{data} is a numerical vector without named elements that vector is turned into
#' a item-weight data frame with the items being vector's indexes.
#' @export
ParetoForWeightedItems <- function( data, normalizeQ = TRUE ) {

  data <- MakeParetoData( data )

  ## At this point data is assumed to have the columns c("Item", "Weight").
  ## assertthat::assert_that( mean( names(data) %in% c("Item", "Weight") ) == 1 )

  ## Just in case. Using sum( Weight, na.rm = T ) makes the very code slow.
  data <- data[complete.cases(data), ]

  ## Aggregation in case we have duplicated names
  if( length(unique(data$Item)) < nrow(data) ) {
    data <-
      data %>%
      dplyr::group_by( Item ) %>%
      dplyr::summarise( Weight = sum(Weight) ) %>%
      dplyr::ungroup()
  }

  data <-
    data %>%
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
#' @param variables The variables to calculate upon.
#' If NULL then it is automatically selected depending on the value given to \code{form}.
#' @param normalizeQ Should the Pareto statistic be normalized with the total or not?
#' @param form In what form \code{data} is.
#' One of NULL, "Long", or "Wide".
#' If NULL then it is the same as "Wide".
#' @return A data frame with columns \code{c("Variable", "Item", "ParetoFraction")}.
#' @details This function works on both long form and wide form data.
#' It redirects to the functions
#' \code{\link{ParetoForLongForm}} and \code{\link{ParetoForWideForm}}
#' respectively.
#' @export
ParetoForVariables <- function( data, variables = NULL, normalizeQ = TRUE, form = "Wide" ) {

  if( is.null(form) || tolower(form) == "long" ) {

    ParetoForLongForm( data = data, variables = variables, normalizeQ = normalizeQ )

  } else {

    ParetoForWideForm( data = data, variables = variables, normalizeQ = normalizeQ )

  }

}


#' Pareto for wide form data.
#' @description Computes the Pareto statistic for each of the specified columns.
#' @param data A data frame.
#' @param variables The variables to calculate upon.
#' If NULL then \code{variables = colnames(data)}.
#' @param normalizeQ Should the Pareto statistic be normalized with the total or not?
#' @return A data frame with columns \code{c("Variable", "Item", "ParetoFraction")}.
#' @details This function works on wide form data.
#' It delegates to the function \code{\link{ParetoForWeightedItems}}.
#' Note that the specified columns can be a mix of categorical and numerical columns.
#' @export
ParetoForWideForm <- function( data, variables = colnames(data), normalizeQ = TRUE ) {

  if( is.matrix(data) && is.numeric(data) ) {

    data <- as.data.frame(data)

    if( is.null(colnames(data)) ) {
      colnames(data) <- make.names(as.character(1:ncol(data)))
    }
  }

  if( !is.data.frame(data) ) {
    stop( "The argument data is expected to be a data frame or a numeric matrix.", call. = TRUE )
  }

  if( is.null(variables) ) { variables = colnames(data) }

  if( sum(variables %in% colnames(data)) < length(variables) ) {
    warning( "Some of the specified column names are unknown.", call. = TRUE )
    variables <- variables[ variables %in% colnames(data) ]
  }


  if( ! is.character(variables) ) {
    stop( "The argument variables is expected to be a non-empty character vector with column names of the argument data.", call. = TRUE )
  }

  purrr::map_dfr( variables, function(x) {
    cbind( Variable = x, ParetoForWeightedItems( data = data[[x]], normalizeQ = normalizeQ ), stringsAsFactors = FALSE )
  })

}


#' Pareto for long form data.
#' @description Computes the Pareto statistic for each of the specified columns.
#' @param data A data frame with columns \code{c("Variable", "Item", "Weight")}.
#' @param variables The variables to calculate upon.
#' If NULL then \code{variables = colnames(data)}.
#' @param normalizeQ Should the Pareto statistic be normalized with the total or not?
#' @return A data frame with columns \code{c("Variable", "Item", "ParetoFraction")}.
#' @details This function works on long form data.
#' It delegates to the function \code{\link{ParetoForWeightedItems}}.
#' The data is partitioned according to \code{data$Variable}.
#' If \code{normalizeQ = TRUE} then for each part \code{data2} the Pareto statistic is computed with the formula:
#' \code{ cumsum(sort(data2$Weight, decreasing=TRUE)) / sum(data2$Weight) }.
#' @export
ParetoForLongForm <- function( data, variables = NULL, normalizeQ = TRUE ) {

  lsExpectedColumnNames <- c("Variable", "Item", "Weight")
  if( !is.data.frame(data ) && sum( lsExpectedColumnNames %in% colnames(data) ) < length(lsExpectedColumnNames) ) {
    stop( paste0( "The argument data is expected to be a data frame with columns: ", paste( lsExpectedColumnNames, collapse = ", " ) ), call. = TRUE )
  }

  purrr::map_dfr( split(data, data$Variable), function(dfX) {
    cbind( Variable = dfX$Variable[[1]], ParetoForWeightedItems( data = dfX[, c("Item", "Weight")], normalizeQ = normalizeQ ), stringsAsFactors = FALSE )
  })

}


#' Pareto for variables. (Obsolete.)
#' @description Computes the Pareto statistic for each of the specified columns.
#' @param data A data frame.
#' @param columnNames The column names of the variables.
#' @param normalizeQ Should the Pareto statistic be normalized with the total or not?
#' @return A data frame with columns \code{c("Variable", "Item", "ParetoFraction")}.
#' @details This function works on wide form data.
#' (The function \code{\link{ParetoForWeightedItems}} works on long form data.)
#' Note that the specified columns can be a mix of categorical and numerical columns.
#' @export
ParetoForVariablesObsolete <- function( data, columnNames = colnames(data), normalizeQ = TRUE ) {

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

#' Plot points with a Pareto frame.
#' @description  Plot the points of a vector and overlay a Pareto grid
#' (without computing the Pareto statistic.)
#' @param data A numeric vector.
#' @param main A string for the title of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param xFraction The Pareto fraction for the x-axis.
#' @param yFraction The Pareto fraction for the y-axis.
#' @param showParetoTicksQ Should the Pareto ticks be shown or not?
#' @param computeStatisticQ Should the Pareto statistic be computed or not?
#' @param ... Additional parameters for \code{\link{base::plot}}.
#' @details This plot can be used when the Pareto statistic is
#' computed with some other functions.
#' (Or for for ad hoc Pareto computations.)
#' If \code{computeStatisticQ = FALSE} then \code{data <- cumsum(rev(sort(data))) / sum(data)}
#' is executed first.
#' @family Pareto plots
#' @export
ParetoFramePlot <- function( data, main = NULL, xlab = "Index", ylab = "ParetoFraction",
                             xFraction = 0.2, yFraction = 0.8, showParetoTicksQ = TRUE,
                             computeStatisticQ = FALSE, ... ) {


  if( ! is.numeric(data) ) {
    stop( "The argument data is expected to be a numerical vector.", call. = TRUE )
  }


  if( computeStatisticQ ) {
    data <- cumsum(rev(sort(data))) / sum(data)
  } else {
    data <- sort(data)
  }

  plot( data, type='l', main=main, xlab=xlab, ylab=ylab, ... )
  grid()
  ## grid(nx=4,ny=4)
  vticks <- c( xFraction*length(data) )
  hticks <- c( yFraction )

  if( showParetoTicksQ ) {
    axis( side = 3, at = vticks, labels = as.character(xFraction) )
    axis( side = 4, at = hticks )
  }

  abline( v = vticks, h = hticks, col = "red", lty = "dotted" )
}


#' Pareto plot for a vector.
#' @description Computes the Pareto statistic for a vector and plots it.
#' @param data A character, factor, or numerical vector.
#' @param ... Additional parameters for \code{\link{ParetoPlotForColumns}}.
#' @details This function is a based on \code{\link{ParetoPlotForColumns}}.
#' @family Pareto plots
#' @export
ParetoPlot <- function( data, ... ) {

  if( !( is.character(data) || is.factor(data) || is.numeric(data) ) ) {
    stop( "The argument data is expected to be a character, factor, or numerical vector.", call. = TRUE )
  }

  varName <- paste(class(data), collapse = " ")
  ParetoPlotForColumns( setNames( data.frame( Value = data), varName ), columnNames = c(varName), ... )
}


#' Pareto plot for variables.
#' @description Make a plot that demonstrates the adherence to the Pareto Principle
#' of a data frame with item names and weight values.
#' @param data A data frame.
#' @param normalizeQ Should the Pareto cumulative values be normalized or not?
#' @param form In what form \code{data} is.
#' One of NULL, "Long", or "Wide".
#' If NULL then it is the same as "Wide".
#' @param separatedPlotsQ Should the plotted Pareto curves be separated or not?
#' @param main A string for the title of the plot.
#' @param scales Sames as \code{scales} of \code{ggplot2::facet_wrap}.
#' @param nrow Sames as \code{nrow} of \code{\link{ggplot2::facet_wrap}}.
#' @param ncol Sames as \code{ncol} of \code{\link{ggplot2::facet_wrap}}.
#' @param ... Parameters for \code{\link{ggplot2::theme}}
#' @details
#' The data frame \code{data} is transformed by
#' \code{ParetoForLongForm} or \code{ParetoForWideForm} according to the
#' value of \code{form}
#' The column \code{data$Value} is used to make the Pareto Principle curves.
#' Each plot has percentage vertical lines.
#' @family Pareto plots
#' @export
ParetoPlotForVariables <- function( data,
                                     normalizeQ = TRUE,
                                     form = "Wide",
                                     separatedPlotsQ = TRUE,
                                     main = NULL, scales = "fixed", nrow = NULL, ncol = NULL, ...  ) {


  pres <- ParetoForVariables( data = data, form = form, normalizeQ = normalizeQ )

  qdf <-
    purrr::map_df( split( pres, pres[["Variable"]] ), function(dfX) {

      if( nrow(dfX) == 0 ) {

        NULL

      } else {

        pres <- cbind( dfX, Index = 1:nrow(dfX) )

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


#' Pareto plot for variables. (Obsolete.)
#' @description Make a plot that demonstrates the adherence to the Pareto Principle
#' of a data frame with item names and weight values.
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
ParetoPlotForVariablesObsolete <- function( data, normalizeQ = TRUE, separatedPlotsQ = TRUE,
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
#' @details This function makes the Pareto plots over a wide form data frame.
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
