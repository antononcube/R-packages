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
#' @import shiny
#' @import shinydashboard
NULL

##===========================================================
## UI page
##===========================================================

#' Time series search interface UI
#' @param tsSMR A time series recommender.
#' @param tsSearchVectors A list of time series search vectors.
#' @return Shiny UI object.
#' @family Time series search interface functions
#' @export
TSCorrSMRMakeUI <- function( tsSMR, tsSearchVectors ) {
  
  dashboardPage(
    dashboardHeader(title = "Time series dashboard"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Nearest neighbors", tabName = "NNs"),
        menuItem("Trend finding", tabName = "TrendFinding")
      )
    ),
    
    dashboardBody(
      
      tabItems(
        
        tabItem( tabName = "NNs",
                 
                 selectInput( "searchID", "Entity:", rownames(tsSMR$SMR$M) ),
                 
                 numericInput( "numberOfNNs", "Number of NNs:", 12 ),
                 
                 fluidRow(
                   column(width = 4,
                          selectInput( "nnsValueColName", "Value plotting:", c( "Raw" = "Value", "Smoothed" = "Value.ma" ) )),
                   column(width = 4,
                          numericInput( "nnsSmoothedWindowSize", "Smoothing window size:", min = 1, max = 60, step = 1, value = 12 ))
                   ),
                 
                 hr(),
                 
                 plotOutput( "entetyNNsPlot", height = "1000px" )
                 
        ),
        
        tabItem( tabName = "TrendFinding",
                 
                 selectInput( "searchVectorName", "Search vector type:", names(tsSearchVectors) ),
                 
                 numericInput( "numberOfSearchResults", "Number of search results:", 12 ),
                 
                 fluidRow(
                   column(width = 4,
                          selectInput( "svecValueColName", "Value plotting:", c( "Raw" = "Value", "Smoothed" = "Value.ma" ) ) ),
                   column(width = 4,
                          numericInput( "svecSmoothedWindowSize", "Smoothing window size:", min = 1, max = 60, step = 1, value = 12 ) )
                 ),
                 
                 selectInput( "searchVectorColor", "Search vector color:", c("blue", "lightblue", "black", "gray10", "gray25", "gray50", "gray75", "gray90" ), selected = "gray75" ),
                 
                 hr(),
                 
                 plotOutput( "searchVectorPlot", height = "150px", width = "550px" ),
                 
                 plotOutput( "searchVectorNNsPlot",  width = "1200px", height = "800px" )
                 
                 
        )
      ) 
    )
  )
}


##===========================================================
## server function
##===========================================================

#' Time series search interface server function
#' @param tsSMR A time series recommender.
#' @param tsSearchVectors A list of time series search vectors.
#' @return Shiny server function.
#' @family Time series search interface functions.
#' @export
TSCorrSMRMakeServerFunction <- function( tsSMR, tsSearchVectors ) {
  
  function(input, output, session)  {
    
    
    recResNNs <- reactive(
      
      TSPSRCorrelationNNs( timeSeriesMat = tsSMR$TSMat, smr = tsSMR$SMR, 
                           itemIDtoNameRules = tsSMR$ItemIDtoNameRules,
                           searchRowID = input$searchID, nrecs = input$numberOfNNs )
      
    )
    
    recResSVec <- reactive(
      
      TSPSRCorrelationNNs( timeSeriesMat = tsSMR$TSMat, smr = tsSMR$SMR, 
                           itemIDtoNameRules = tsSMR$ItemIDtoNameRules,
                           searchVector = tsSearchVectors[[ input$searchVectorName ]], nrecs = input$numberOfSearchResults )
      
    )
    
    recResNNsExtended <- reactive( 
      
      setNames( SMRSparseMatrixToTriplets( smat = tsSMR$TSMat ), c("Entity", "TimeIntervalBoundaryName", "Value" ) ) %>% 
        # mutate( TimeIntervalBoundary = as.POSIXct( TimeIntervalBoundary, format="%Y-%m-%d") ) %>%
        mutate( TimeIntervalBoundary = tsSMR$TIBNameToTIBRules[ TimeIntervalBoundaryName ] ) %>%
        dplyr::filter( Entity %in% recResNNs()$ItemID ) %>%
        dplyr::inner_join( recResNNs(), by = c("Entity" = "ItemID" ) ) %>% 
        dplyr::arrange( Score, Entity, TimeIntervalBoundary) %>%
        dplyr::group_by( Entity ) %>%
        dplyr::mutate( Value.ma = RcppRoll::roll_mean(Value, input$nnsSmoothedWindowSize, align="right", fill=0) ) %>%
        dplyr::ungroup()
      
    )
    
    recResSVecExtended <- reactive( 
      
      setNames( SMRSparseMatrixToTriplets( smat = tsSMR$TSMat ), c("Entity", "TimeIntervalBoundaryName", "Value" ) ) %>% 
        # mutate( TimeIntervalBoundary = as.POSIXct( TimeIntervalBoundary, format="%Y-%m-%d") ) %>%
        mutate( TimeIntervalBoundary = tsSMR$TIBNameToTIBRules[ TimeIntervalBoundaryName ] ) %>%
        dplyr::filter( Entity %in% recResSVec()$ItemID ) %>%
        dplyr::inner_join( recResSVec(), by = c("Entity" = "ItemID" ) ) %>% 
        dplyr::arrange( Score, Entity, TimeIntervalBoundary) %>%
        dplyr::group_by( Entity ) %>%
        dplyr::mutate( Value.ma = RcppRoll::roll_mean(Value, input$svecSmoothedWindowSize, align="right", fill=0) ) %>%
        dplyr::ungroup()
      
    )
    
    ## Entity NNs plot 
    output$entetyNNsPlot <- renderPlot( {
      
      ggplot2::ggplot( recResNNsExtended() ) +
        ggplot2::geom_line( ggplot2::aes_string( x = "TimeIntervalBoundary", y = input$nnsValueColName, color = "ItemName" ), na.rm = T ) +
        ggplot2::facet_wrap( ~ reorder(ItemName, -Score), ncol = 2, scales = "free" )
      
    })
    
    
    searchVecPlotDF <- reactive(
      data.frame( TimeIntervalBoundary = tsSMR$TIBNameToTIBRules[ colnames(tsSMR$TSMat) ], Value = tsSearchVectors[[input$searchVectorName]] ) 
    )
    
    ## Search vector plot 
    output$searchVectorPlot <- renderPlot( {
      
      ggplot2::ggplot( searchVecPlotDF() ) +
        ggplot2::geom_line( ggplot2::aes( x = TimeIntervalBoundary, y = Value ), na.rm = T )
      
    })
    
    
    ## Search vector NNs plot 
    output$searchVectorNNsPlot <- renderPlot( {
      
      # ggplot( recResSVecExtended() ) +
      #   geom_line( aes( x = TimeIntervalBoundary, y = Value.ma, color = ItemName ), na.rm = T ) +
      #   facet_wrap( ~ reorder(ItemName, -Score), ncol = 2, scales = "free" )
      
      valueColumnName <- input$svecValueColName
      searchVecPlotDF2 <- 
        ddply( recResSVecExtended(), "ItemName", function(x) {
          vec = searchVecPlotDF()$Value
          vec = ( vec - min(vec) ) / ( max(vec) - min(vec) ) 
          vec = vec * (max(x[[valueColumnName]]) - min(x[[valueColumnName]])) + min(x[[valueColumnName]])
          res <- data.frame( Score = 1, TimeIntervalBoundary = searchVecPlotDF()$TimeIntervalBoundary, Value.ma = vec, ItemName = x$ItemName[[1]] ) 
          colnames(res) <- c( "Score", "TimeIntervalBoundary", valueColumnName, "ItemName" )
          res
        } )
      
      ggplot2::ggplot( recResSVecExtended()  ) +
        ggplot2::geom_line( ggplot2::aes_string( x = "TimeIntervalBoundary", y = valueColumnName, color = "ItemName" ), na.rm = T ) +
        ggplot2::facet_wrap( ~ reorder(ItemName, -Score), ncol = 2, scales = "free" ) +
        ggplot2::geom_line( data = searchVecPlotDF2, ggplot2::aes_string( x = "TimeIntervalBoundary", y = valueColumnName), color = input$searchVectorColor )
      
      
    })
    
  }
}


##===========================================================
## Make shiny app
##===========================================================

#' Creation of time series search interface
#' @param tsSMR A time series recommender.
#' @param tsSearchVectors A list of time series search vectors.
#' @return Shiny app object.
#' @family Time series search interface functions
#' @export
TSCorrSMRCreateSearchInterface <- function( tsSMR, tsSearchVectors = NULL ) {
  
  if( is.null(tsSearchVectors) ) {
    tsSearchVectors <- MakeTimeSeriesSearchVectors( tsSMR$TSMat )
  }
  
  if( is.null(tsSMR$ItemIDtoNameRules) ) {
    tsSMR$ItemIDtoNameRules <- setNames( rownames(tsSMR$SMR$M), rownames(tsSMR$SMR$M) )
  }
  
  if( is.null(tsSMR$TIBNameToTIBRules) ) {
    tsSMR$TIBNameToTIBRules <- setNames( 1:ncol(tsSMR$TSMat), colnames(tsSMR$TSMat))
  }
  
  shiny::shinyApp( ui = TSCorrSMRMakeUI( tsSMR = tsSMR, tsSearchVectors = tsSearchVectors ), 
                   server = TSCorrSMRMakeServerFunction( tsSMR = tsSMR, tsSearchVectors = tsSearchVectors )
  )
}