##=======================================================================================
## Time series recommender interface, server side
## Copyright (C) 2017  Anton Antonov
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
## The code in this interface is to be used with [1].
##
## In order to run this interface.
##
## 1. Create a time series SMR object.
##    Assign to the variable 'tsSMR' .
##
## 1.1. The object tsSMR is assumed to have a time series matrix
##      assigned to tsSMR$TSMat .
##
## 1.2. The object tsSMR is assumed to have a vector with named elements that maps column names to time
##      interval boundaries assigned to tsSMR$tibNameToTIBRules .
##
## 1.3. The object tsSMR is assumed to have a vector with named elements that maps itemID to item names
##      assigned to the variable tsSMR$ItemIDtoNameRules .
##
## 2. Create a list of search vectors to be used in the interface.
##    Each element of that list is named. Assign to 'tsSearchVectors' .
##    Such vector can be created with:
##       tsSearchVectors <- MakeTimeSeriesSearchVectors( tsSMR$TSMat )
##    using a definition from [1] (which also defines TSPSRCorrelationNNs.)
##
##=======================================================================================
## References
##=======================================================================================
## [1] Anton Antonov, Time series recommender framework in R, (2017), GitHub,
##     URL: https://github.com/antononcube/R-packages/blob/master/SparseMatrixRecommender/R/TimeSeriesRecommender.R .
##=======================================================================================


#' @import dplyr
#' @import ggplot2
#' @import RcppRoll
#' @import stringr
#' @import shiny
#' @import shinydashboard
#' @import shinythemes
NULL

##===========================================================
## UI page
##===========================================================

#' Time series search interface UI
#' @description Creates the Shiny UI function for a time series search interface.
#' @param tsSMR A time series recommender.
#' @param tsSearchVectors A list of time series search vectors.
#' @param initNNs Initial number nearest neighbors.
#' @param initNCols Initial number of columns.
#' @param plotOutputHeight Plot output height for \code{\link{plotOutput}}.
#' @param dashboardTitle Dashboard title.
#' @param noteText Note text to display.
#' @param theme Shiny-theme to apply.
#' @return Shiny UI object.
#' @family Time series search interface functions
#' @export
TSCorrSMRMakeUI <- function( tsSMR, tsSearchVectors, initNNs = 12, initNCols = 2, dashboardTitle = "Time series search engine", plotOutputHeight = "1000px", noteText = NULL, theme = "cerulean" ) {

  if( is.null(initNNs) ) { initNNs = 12 }
  if( is.null(initNCols) ) { initNCols = 2 }
  if( is.null(dashboardTitle) ) { dashboardTitle = "Time series search engine" }


  fluidPage(
    theme = shinythemes::shinytheme(theme),

    navbarPage(

      title = dashboardTitle,

      tabPanel( title = "Nearest Neighbors",

                sidebarLayout(
                  sidebarPanel =
                    sidebarPanel(
                      width = 3,
                      selectInput( "searchID", "Entity:", rownames(tsSMR$SMR$M) ),
                      textInput( "filterRowIDs", "Entities filter pattern:", ""),
                      sliderInput( "numberOfNNs", "Number of nearest neighbors:", min = 1, max = 100, step = 1, value = initNNs ),
                      sliderInput( "nnsNCol", "Number of graphics columns:", min = 1, max = 12, step = 1, value = initNCols ),
                      hr(),
                      radioButtons( inputId = "nnsMethod",
                                   label = "Correlation method:",
                                   choices = c( "Dot" = "dot", "Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall" ),
                                   selected = "pearson",
                                   inline = TRUE ),
                      radioButtons( inputId = "nnsScales",
                                   label = "Graphics scales:",
                                   choices = c( "Fixed X & Y" = "fixed", "Free Y" = "free_y", "Free X" = "free_x" ),
                                   selected = "free_y",
                                   inline = TRUE ),
                      hr(),
                      radioButtons( inputId = "nnsValueColName",
                                    label = "Value plotting:",
                                    choices = c( "Raw" = "Value", "Smoothed" = "Value.ma" ),
                                    inline = TRUE ),
                      sliderInput( "nnsSmoothedWindowSize", "Smoothing window size:", min = 1, max = 60, step = 1, value = 12 ),
                      fluid = TRUE
                    ),

                mainPanel( width = 9, plotOutput( "entetyNNsPlot", height = plotOutputHeight), fluid = TRUE )

      )),

      tabPanel( title = "Trend Finding",

                sidebarLayout(
                  sidebarPanel =
                    sidebarPanel(
                      width = 3,
                      selectInput( "searchVectorName", "Search vector type:", names(tsSearchVectors) ),
                      textInput( "svecFilterRowIDs", "Entities filter pattern:", ""),
                      radioButtons( inputId = "searchVectorColor",
                                    label = "Search vector color:",
                                    choices = c("blue", "lightblue", "black", "gray10", "gray25", "gray50", "gray75", "gray90" ),
                                    selected = "gray75",
                                    inline = TRUE ),
                      hr(),
                      sliderInput( "numberOfSearchResults", "Number of nearest neighbors:", min = 1, max = 100, step = 1, value = initNNs ),
                      sliderInput( "svecNCol", "Number of graphics columns:", min = 1, max = 12, step = 1, value = initNCols ),
                      hr(),
                      radioButtons( inputId = "svecMethod",
                                   label = "Correlation method:",
                                   choices = c( "Dot" = "dot", "Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall" ),
                                   selected = "pearson",
                                   inline = TRUE ),
                      radioButtons( inputId = "svecScales",
                                   label = "Graphics scales:",
                                   choices = c( "Fixed X & Y" = "fixed", "Free Y" = "free_y", "Free X" = "free_x" ),
                                   selected = "free_y",
                                   inline = TRUE ),
                      hr(),
                      radioButtons( inputId = "svecValueColName",
                                    label = "Value plotting:",
                                    choices = c( "Raw" = "Value", "Smoothed" = "Value.ma"),
                                    inline = TRUE ),
                      sliderInput( "svecSmoothedWindowSize", "Smoothing window size:", min = 1, max = 60, step = 1, value = 12 ),
                      fluid = TRUE
                    ),

                  mainPanel(
                    width = 9,
                    plotOutput( "searchVectorPlot", height = "150px", width = "550px" ),
                    hr(),
                    plotOutput( "searchVectorNNsPlot",  height = plotOutputHeight ),
                    fluid = TRUE
                  )
                )

      ),

      tabPanel( title = "Notes",

                infoBox(title = NULL,
                        value = noteText,
                        subtitle = NULL,
                        icon = shiny::icon("sticky-note"), color = "aqua", width = 9,
                        href = NULL, fill = FALSE)
      )

    )
  )
}


##===========================================================
## Server function
##===========================================================

#' Time series search interface server function
#' @description Creates the Shiny server function for a time series search interface.
#' @param tsSMR A time series recommender.
#' @param tsSearchVectors A list of time series search vectors.
#' @param roundDigits Number of decimal places for \code{\link{round}}.
#' (Used for making the \code{ggplot} panel names.)
#' @return Shiny server function.
#' @family Time series search interface functions
#' @export
TSCorrSMRMakeServerFunction <- function( tsSMR, tsSearchVectors, roundDigits = 6 ) {

  function(input, output, session)  {


    recResNNs <- reactive({

      if( nchar(input$filterRowIDs) == 0 || input$filterRowIDs == ".*" ) {
        fids <- NULL
      } else{
        fids <- grep( pattern = input$filterRowIDs, x = rownames(tsSMR$TSMat), value = T )
      }

      SparseMatrixRecommender::TSPSRCorrelationNNs( timeSeriesMat = tsSMR$TSMat, smr = tsSMR$SMR,
                                                    itemIDtoNameRules = tsSMR$ItemIDtoNameRules,
                                                    searchRowID = input$searchID, nrecs = input$numberOfNNs,
                                                    method = input$nnsMethod,
                                                    filterRowIDs = fids )

    })

    recResSVec <- reactive({

      if( nchar(input$svecFilterRowIDs) == 0 || input$svecFilterRowIDs == ".*" ) {
        fids <- NULL
      } else{
        fids <- grep( pattern = input$svecFilterRowIDs, x = rownames(tsSMR$TSMat), value = T )
      }

      SparseMatrixRecommender::TSPSRCorrelationNNs( timeSeriesMat = tsSMR$TSMat, smr = tsSMR$SMR,
                                                    itemIDtoNameRules = tsSMR$ItemIDtoNameRules,
                                                    searchVector = tsSearchVectors[[ input$searchVectorName ]], nrecs = input$numberOfSearchResults,
                                                    method = input$svecMethod,
                                                    filterRowIDs = fids )

    })

    recResNNsExtended <- reactive(

      setNames( SMRSparseMatrixToTriplets( smat = tsSMR$TSMat ), c("Entity", "TimeIntervalBoundaryName", "Value" ) ) %>%
        # mutate( TimeIntervalBoundary = as.POSIXct( TimeIntervalBoundary, format="%Y-%m-%d") ) %>%
        dplyr::mutate( TimeIntervalBoundary = tsSMR$TIBNameToTIBRules[ TimeIntervalBoundaryName ] ) %>%
        dplyr::filter( Entity %in% recResNNs()$ItemID ) %>%
        dplyr::inner_join( recResNNs(), by = c("Entity" = "ItemID" ) ) %>%
        dplyr::arrange( Score, Entity, TimeIntervalBoundary) %>%
        dplyr::group_by( Entity ) %>%
        dplyr::mutate( Value.ma = RcppRoll::roll_mean(Value, input$nnsSmoothedWindowSize, align="right", fill=0) ) %>%
        dplyr::mutate( ItemName.Score = paste( ItemName, round(x = Score, digits = roundDigits), sep = " : ") ) %>%
        dplyr::ungroup()

    )

    recResSVecExtended <- reactive(

      setNames( SMRSparseMatrixToTriplets( smat = tsSMR$TSMat ), c("Entity", "TimeIntervalBoundaryName", "Value" ) ) %>%
        # mutate( TimeIntervalBoundary = as.POSIXct( TimeIntervalBoundary, format="%Y-%m-%d") ) %>%
        dplyr::mutate( TimeIntervalBoundary = tsSMR$TIBNameToTIBRules[ TimeIntervalBoundaryName ] ) %>%
        dplyr::filter( Entity %in% recResSVec()$ItemID ) %>%
        dplyr::inner_join( recResSVec(), by = c("Entity" = "ItemID" ) ) %>%
        dplyr::arrange( Score, Entity, TimeIntervalBoundary) %>%
        dplyr::group_by( Entity ) %>%
        dplyr::mutate( Value.ma = RcppRoll::roll_mean(Value, input$svecSmoothedWindowSize, align="right", fill=0) ) %>%
        dplyr::mutate( ItemName.Score = paste( ItemName, round(x = Score, digits = roundDigits), sep = " : ") ) %>%
        dplyr::ungroup()

    )

    ## Entity NNs plot
    output$entetyNNsPlot <- renderPlot( {

      ggDF <- recResNNsExtended()
      facDF <- unique(ggDF[, c("ItemName.Score", "Score")])
      facDF <- facDF[ order(-facDF$Score), ]
      ggDF$ItemName.Score <- factor( x = ggDF$ItemName.Score, levels = facDF$ItemName.Score )

      ggplot2::ggplot( ggDF ) +
        ggplot2::geom_line( ggplot2::aes_string( x = "TimeIntervalBoundary", y = input$nnsValueColName, color = "ItemName.Score" ), na.rm = T ) +
        ggplot2::facet_wrap( ~ItemName.Score, ncol = input$nnsNCol, scales = input$nnsScales )
      # if( is.numeric(ggDF$TimeIntervalBoundary) ) { ggplot2::scale_x_continuous(position = 'top') } else { ggplot2::scale_x_datetime(position = 'top') }

    })


    searchVecPlotDF <- reactive(
      data.frame( TimeIntervalBoundary = tsSMR$TIBNameToTIBRules[ colnames(tsSMR$TSMat) ], Value = tsSearchVectors[[input$searchVectorName]] )
    )

    ## Search vector plot
    output$searchVectorPlot <- renderPlot( {

      ggplot2::ggplot( searchVecPlotDF() ) +
        ggplot2::geom_line( ggplot2::aes( x = TimeIntervalBoundary, y = Value ), na.rm = T ) +
        ggplot2::ggtitle( "Search vector" )

    })


    ## Search vector NNs plot
    output$searchVectorNNsPlot <- renderPlot( {

      # ggplot( recResSVecExtended() ) +
      #   geom_line( aes( x = TimeIntervalBoundary, y = Value.ma, color = ItemName ), na.rm = T ) +
      #   facet_wrap( ~ reorder(ItemName, -Score), ncol = 2, scales = "free" )

      valueColumnName <- input$svecValueColName
      searchVecPlotDF2 <-
        purrr::map_df( split( recResSVecExtended(), recResSVecExtended()$ItemName.Score), function(x) {
          vec = searchVecPlotDF()$Value
          vec = ( vec - min(vec) ) / ( max(vec) - min(vec) )
          vec = vec * (max(x[[valueColumnName]]) - min(x[[valueColumnName]])) + min(x[[valueColumnName]])
          res <- data.frame( Score = 1, TimeIntervalBoundary = searchVecPlotDF()$TimeIntervalBoundary, Value.ma = vec, ItemName.Score = x$ItemName.Score[[1]] )
          colnames(res) <- c( "Score", "TimeIntervalBoundary", valueColumnName, "ItemName.Score" )
          res
        } )

      ggDF2 <- recResSVecExtended()
      facDF2 <- unique(ggDF2[, c("ItemName.Score", "Score")])
      facDF2 <- facDF2[ order(-facDF2$Score), ]
      ggDF2$ItemName.Score <- factor( x = ggDF2$ItemName.Score, levels = facDF2$ItemName.Score )

      searchVecPlotDF2$ItemName.Score <- factor( x = searchVecPlotDF2$ItemName.Score, levels = facDF2$ItemName.Score )

      ggplot2::ggplot( ggDF2  ) +
        ggplot2::geom_line( ggplot2::aes_string( x = "TimeIntervalBoundary", y = valueColumnName, color = "ItemName.Score" ), na.rm = T ) +
        ggplot2::facet_wrap( ~ ItemName.Score, ncol = input$svecNCol, scales = input$svecScales ) +
        ggplot2::geom_line( data = searchVecPlotDF2, ggplot2::aes_string( x = "TimeIntervalBoundary", y = valueColumnName), color = input$searchVectorColor )
      #if( is.numeric(ggDF2$TimeIntervalBoundary) ) { ggplot2::scale_x_continuous(position = 'top') } else { ggplot2::scale_x_datetime(position = 'top') }

    })

  }
}


##===========================================================
## Make shiny app
##===========================================================

#' Creation of time series search interface
#' @description Creates Shiny interface for a given time series recommender and
#' optional search time series shapes.
#' @param tsSMR A time series recommender.
#' @param tsSearchVectors A list of time series search vectors.
#' @param initNNs Initial number nearest neighbors.
#' @param initNCols Initial number of columns.
#' @param plotOutputHeight Plot output height for \code{\link{plotOutput}}.
#' @param roundDigits Number of decimal places for \code{\link{round}}.
#' @param dashboardTitle Dashboard title.
#' @param noteText Note text to display.
#' @param theme Shiny-theme to apply.
#' (Used for making the \code{ggplot} panel names.)
#' @return Shiny app object.
#' @family Time series search interface functions
#' @export
TSCorrSMRCreateSearchInterface <- function( tsSMR, tsSearchVectors = NULL,
                                            initNNs = 12, initNCols = 2, plotOutputHeight = "1000px",
                                            roundDigits = 6,
                                            dashboardTitle = "Time series search engine",
                                            noteText = NULL,
                                            theme = "cerulean" ) {

  if( is.null(tsSearchVectors) ) {
    tsSearchVectors <- MakeTimeSeriesSearchVectors( tsSMR$TSMat )
  }

  if( is.null(tsSMR$ItemIDtoNameRules) ) {
    tsSMR$ItemIDtoNameRules <- setNames( rownames(tsSMR$SMR$M), rownames(tsSMR$SMR$M) )
  }

  if( is.null(tsSMR$TIBNameToTIBRules) ) {
    tsSMR$TIBNameToTIBRules <- setNames( 1:ncol(tsSMR$TSMat), colnames(tsSMR$TSMat))
  }

  shiny::shinyApp( ui = TSCorrSMRMakeUI( tsSMR = tsSMR, tsSearchVectors = tsSearchVectors,
                                         initNNs = initNNs, initNCols = initNCols, plotOutputHeight = plotOutputHeight,
                                         dashboardTitle = dashboardTitle,
                                         noteText = noteText,
                                         theme = theme ),
                   server = TSCorrSMRMakeServerFunction( tsSMR = tsSMR, tsSearchVectors = tsSearchVectors, roundDigits = roundDigits )
  )
}
