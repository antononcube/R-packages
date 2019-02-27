##=======================================================================================
## Pareto law functions in R
## Copyright (C) 2015-2017  Anton Antonov
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

#' Pareto for a categorical vector.
#' @description Make a plot that demonstrates the adherence to Pareto Principle
#' of the unique entries of a categorical vector.
#' @param dataVec A vector with categorical entries.
#' @param plotTitle A string for the title of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @family Pareto plots
#' @export
ParetoLawForCountsBasePlot <- function( dataVec, main = NULL, xlab="|levels|", ylab="%", ... ) {
  taCounts <- plyr::count(dataVec)
  taCounts <- taCounts[rev(order(taCounts$freq)),]
  ParetoLawBasePlot( taCounts$freq, main, xlab=xlab, ylab=ylab, ...)
}


#' Pareto for a numerical vector.
#' @description Make a plot that demonstrates the adherence to Pareto Principle of
#' a numerical vector with contingency values (counts).
#' @param dataVec A numerical vector.
#' @param plotTitle A string for the title of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @details Pareto plots
#' @export
ParetoLawBasePlot <- function( dataVec, main = NULL, xlab="|levels|", ylab="%", xFraction = 0.2, yFraction = 0.8, ... ) {
  dataVec <- rev( sort(dataVec) )
  plot( cumsum( dataVec ) / sum( dataVec ), type='l', main=main, xlab=xlab, ylab=ylab, ... )
  grid()
  ## grid(nx=4,ny=4)
  abline( v=c(xFraction*length(dataVec)), h=c(yFraction), col="red", lty="dotted" )
}

#' Pareto plot for name-value pairs.
#' @description Make a plot that demonstrates the adherence to the Pareto law of a
#' a data frame with item names and weight values.
#' @param data A two column data frame with the first column of a variable name
#' and second column a numerical vector \code{c("Name", "Value")}.
#' @param normalizeQ Should the Pareto cumulative values be normalized or not?
#' @param separatedPlotsQ Should the plotted Pareto curves be separated or not?
#' @param main A string for the title of the plot.
#' @param scales Sames as \code{scales} of \code{ggplot2::facet_wrap}.
#' @param nrow Sames as \code{nrow} of \code{ggplot2::facet_wrap}.
#' @param ncol Sames as \code{ncol} of \code{ggplot2::facet_wrap}.
#' @details
#' The data frame \code{data} is split by \code{data$Name}.
#' The column \code{data$Value} is used to make the Pareto Principle curves.
#' Each plot has percentage vertical lines.
#' @details Pareto plots
#' @export
ParetoLawPlot <- function( data, normalizeQ = TRUE, separatedPlotsQ = TRUE,
                             main = NULL, scales = "fixed", nrow = NULL, ncol = NULL, ...  ) {

  if( !is.data.frame(data) ) {
    stop( "The argument data is expected to be a data frame.", call. = TRUE )
  }

  qdf <-
    purrr::map_df( split( data, data[["Name"]] ), function(x) {
      dataVec <- rev( sort(x$Value) )
      if(normalizeQ) {
        dataVec <- cumsum( dataVec ) / sum( dataVec )
      } else {
        dataVec <- cumsum( dataVec )
      }
      pres <- data.frame( Name = x$Name[[1]], Index = 1:nrow(x), ParetoFraction = dataVec, stringsAsFactors = FALSE )
      cbind( pres,
             p10 = 0.1*nrow(pres), p20 = 0.2*nrow(pres), p30 = 0.3*nrow(pres),  p40 = 0.4*nrow(pres),  p50 = 0.5*nrow(pres) )
    })

  if( separatedPlotsQ ) {

    ggplot2::ggplot(qdf) +
      ggplot2::geom_line(  ggplot2::aes( x = Index, y = ParetoFraction) ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p10), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p20), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p30), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p40), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p50), linetype = 3 ) +
      ggplot2::facet_wrap( ~ Name, scales = scales, nrow = nrow, ncol = ncol  ) +
      ggplot2::theme( ... )

  } else {

    ggplot2::ggplot(qdf) +
      ggplot2::geom_line(  ggplot2::aes( x = Index, y = ParetoFraction, color = Name) ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p10), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p20), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p30), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p40), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p50), linetype = 3 ) +
      ggplot2::theme( ... )

  }
}

#' Pareto statistics computation.
#' @description Sorts the tally of given categorical data descendingly
#' and computes the list of the cumulative sums.
#' @param dataVec A vector with categorical entries.
#' @param weightVec A vector of weights for the values of \code{dataVec};
#' see the argument wt_var of \code{plyr::count}.
#' @export
ParetoLawData <- function( dataVec, weightsVec = NULL ) {

  if ( is.numeric(dataVec) ) {
    stop("The argument dataVec is expected to be a categorical vector.", call. = TRUE )
  }

  if ( is.null(weightsVec) ) {
    dTally <- plyr::count( dataVec )
  } else {
    weightsVec[ is.na(weightsVec) ] <- 0
    dTally <- plyr::count( df = data.frame( dataVec = dataVec, weightsVec = weightsVec ), vars = "dataVec", wt_var = "weightsVec" )
    dTally <- dTally[ !is.na( dTally[[1]] ), ]
  }
  if( class(dataVec) == "character" ) { dTally[[1]] <- as.character(dTally[[1]]) }
  dTally <- dTally[ rev(order(dTally[,c(2)])), ]
  cumSums <- cumsum(dTally[,c(2)])/sum(dTally[,c(2)])
  data.frame(cbind( dTally[1], ParetoFraction = cumSums ))
}


#' Pareto statistics for specified columns.
#' @description Apply \code{\link{ParetoLawData}} over a list of names.
#' @param data A data frame.
#' @param columnNames A list of column names corresponding to categorical columns in \code{data}.
#' @export
ParetoLawDataForColumns <- function( data, columnNames ) {
  res <- purrr::map(columnNames, function(c) {t<-ParetoLawData(data[[c]]); cbind(1:length(t[,1]), t[,2])})
  names(res) <- columnNames
  res
}


#' Top Pareto items determination.
#' @description Finds the Pareto items in a column of a data frame.
#' @param data A data frame.
#' @param colName The column name of items to be counted.
#' @param paretoFraction A number between 0 and 1 specifying the Pareto fraction.
#' @return A data frame with columns c( "Item", "Score", "ParetoFraction" ).
#' @family Pareto items
#' @export
ParetoItems <- function( data, colName, paretoFraction ) {

  paretoItemsCount <- plyr::count( data[colName] )
  paretoItemsCount[[1]] <- as.character( paretoItemsCount[[1]] )

  paretoItemsCount <- paretoItemsCount[ order( -paretoItemsCount[,2] ), ]
  cumSums <- cumsum( paretoItemsCount[,2] ) / sum( paretoItemsCount[,2] )
  paretoItemsCount <- cbind( paretoItemsCount, cumSums = cumSums, stringsAsFactors = FALSE )

  paretoItems <- paretoItemsCount[[1]][ paretoItemsCount$cumSums <= paretoFraction ]

  paretoItemsCount <- paretoItemsCount[ paretoItemsCount[[1]] %in% paretoItems, ]
  paretoItemsCount <- paretoItemsCount[ order(- paretoItemsCount$freq), ]
  names(paretoItemsCount) <- c( "Item", "Score", "ParetoFraction" )

  paretoItemsCount
}


#' Positions of top Pareto items determination.
#' @description Finds the positions of the Pareto items in a column of a data frame.
#' @param dataVec A data vector.
#' @param paretoFraction A number between 0 and 1 specifying the Pareto fraction.
#' @return A data frame with columns c( "Index", "Score", "ParetoFraction" ).
#' @family Pareto items
#' @export
ParetoPositions <- function( dataVec, paretoFraction = 1 ) {

  paretoItemsCount <- data.frame( Index = 1:length(dataVec), Score = dataVec )
  paretoItemsCount <- paretoItemsCount[ rev( order( paretoItemsCount[,2] ) ), ]
  cumSums <- cumsum( paretoItemsCount[,2] ) / sum( paretoItemsCount[,2] )
  paretoItemsCount <- cbind( paretoItemsCount, ParetoFraction = cumSums )

  paretoItems <- paretoItemsCount[[1]][ paretoItemsCount$ParetoFraction <= paretoFraction ]

  paretoItemsCount <- paretoItemsCount[ paretoItemsCount[[1]] %in% paretoItems, ]
  paretoItemsCount <- paretoItemsCount[ order( -paretoItemsCount$Score ), ]
  names(paretoItemsCount) <- c( "Index", "Score", "ParetoFraction" )

  paretoItemsCount
}

#' Pareto Law plots over a data frame columns.
#' @description Apply \code{\link{ParetoLawData}} function over a list of names
#' and make a multi-panel ggplot.
#' @param data A data frame.
#' @param columnNames A list of column names corresponding to categorical columns in \code{data}.
#' @param ... Arguments for \code{ggplot2::facet_wrap}.
#' @details This function make the Pareto plots over a wide form data frame.
#' The function \code{\link{ParetoLawPlot}} makes the plots over long form data.
#' @details Pareto plots
#' @export
ParetoLawPlotForColumns <- function( data, columnNames = colnames(data), weights = NULL, ... ) {

  if( is.null(weights) ) {
    weightsVec <- NULL
  } else if ( is.character(weights) && ( weights %in% colnames(data) ) ) {
    weightsVec <- data[[weights]]
  } else if ( is.numeric(weights) && length(weights) == nrow(data) ) {
    weightsVec <- weights
  } else {
    stop( "The argument weights is expected to be NULL, a column name of the argument data, or a numerical vector with length equal to nrow(data).", call. = T)
  }

  qdf <-
    purrr::map_df( columnNames, function(cn) {
      pres <- ParetoLawData( data[[cn]], weightsVec = weightsVec )
      cbind( ColumnName = cn, Index = 1:nrow(pres), pres,
             p10 = 0.1*nrow(pres), p20 = 0.2*nrow(pres), p30 = 0.3*nrow(pres),  p40 = 0.4*nrow(pres),  p50 = 0.5*nrow(pres) )
    })

  ggplot(qdf) +
    geom_line( aes( x = Index, y = ParetoFraction) ) +
    geom_vline( aes( xintercept = p10), linetype = 3 ) +
    geom_vline( aes( xintercept = p20), linetype = 3 ) +
    geom_vline( aes( xintercept = p30), linetype = 3 ) +
    geom_vline( aes( xintercept = p40), linetype = 3 ) +
    geom_vline( aes( xintercept = p50), linetype = 3 ) +
    facet_wrap( ~ ColumnName, ... )
}
