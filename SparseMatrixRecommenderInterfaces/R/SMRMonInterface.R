##=======================================================================================
## General SMRMon Interface
## Copyright (C) 2017, 2019  Anton Antonov
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
## This Shiny interface is made to be used with the recommender system
## implemented with the packages:
##
##    https://github.com/antononcube/R-packages/tree/master/SparseMatrixRecommender
##
##    https://github.com/antononcube/R-packages/tree/master/SMRMon-R
##
## The two central objects are:
## 1. 'itemData' -- a data frame with rows corresponding to items, and
## 2. 'smrObj' -- a sparse matrix recommender object.
##
## The following variables have to be set:
## 1. searchColName -- which column of itemData is going to be used for searching
##      (e.g. "title" );
## 2. itemDataIDColName -- which column of itemData is with item IDs;
##      these IDs are also rownames of smrObj$M;
## 3. itemDataColNames -- which column names of itemData should be used in the search
##      and recommendation results.
## 4. itemListIDsSplitPattern -- a split pattern for the separator of the items list
##      and ratings list. By default is "\\W". (But "," should be used if the row IDs
##      have white spaces in them.)
##
##=======================================================================================


#' @import DT
#' @import purrr
#' @import stringr
#' @import shiny
#' @import shinydashboard
#' @import SparseMatrixRecommender
#' @import SMRMon
NULL

##===========================================================
## UI function
##===========================================================

#' SMRMon interface UI
#' @description Creates the Shiny UI function for a sparse matrix recommender SMRMon interface.
#' @param smrObj A SMRMon object.
#' @param dashboardTheme A string specifying the dashbaard theme.
#' See \code{\link{dashboardthemes::shinyDashboardThemes}}.
#' Ignored if the package \code{dashboardthemes} is not installed.
#' @return Shiny UI object.
#' @family SMRMon interface functions
#' @export
SMRMonMakeUI <- function( smrObj, dashboardTheme = NULL ) {


  if( !( is.null(dashboardTheme) ||
         is.character(dashboardTheme) && length(dashboardTheme) == 1 ) ) {
    warning( "Ignoring the argument dashboardTheme.", call. = TRUE )
    dashboardTheme <- NULL
  }

  shinyUI(
    dashboardPage(
      dashboardHeader( title = "SMRMon Dashboard" ),

      dashboardSidebar(
        sidebarMenu(
          menuItem( "SMRMon Object", tabName = "SMRMonObject", icon = icon("map") ),
          menuItem( "Search & history", tabName = "SearchAndHistory", icon = icon("search-plus") ),
          menuItem( "Recommendations", tabName = "Recommendations", icon = icon("blender") ),
          menuItem( "Statistics", tabName = "Statistics", icon = icon("chart-bar") ),
          hr(),
          menuItem( "Sliders", tabName = "Sliders", icon = icon("sliders-h"),
                    uiOutput( "multiSliders" )
          )
        )
      ),

      dashboardBody(

        ### Changing dashboard theme
        if( ! is.null(dashboardTheme) && exists("shinyDashboardThemes") ) {
          dashboardthemes::shinyDashboardThemes( theme = dashboardTheme )
        },

        tabItems(

          ## SMRMon object oboarding
          tabItem( tabName = "SMRMonObject",
                   fluidRow(

                     h2( "SMRMon object and interpretation data summaries" ),

                     box(
                       title = "Properties summary",

                       verbatimTextOutput("smrSummary"),

                       div( DT::dataTableOutput("tagTypeRangesTable"), style = "font-size: 75%; width: 75%"),

                       width = 6
                     ),

                     box(
                       title = "Interpretation data summary",

                       verbatimTextOutput("itemDataSummary"),

                       width = 6
                     )

                   )
          ),


          ## Search and history accumulation.
          tabItem( tabName = "SearchAndHistory",
                   fluidRow(

                     h2( "Search and history accumulation" ),

                     fluidRow(

                       box(
                         title = "Search",

                         textInput( inputId = "searchString", label = "Search string:",
                                    value = ".*",
                                    placeholder =  "pattern | <tag-type>:<tag>"),

                         checkboxInput( inputId = "fixedSearhPattern", label = "Fixed ?:", value = T ),

                         width = 6
                       ),


                       box(
                         title = "History items and ratings",

                         textInput( inputId = "itemList", label = "Item list:", value = "10" ),

                         textInput( inputId = "itemRatings", label = "Item star ratings:", value = "3"),

                         width = 6
                       )

                     ),

                     fluidRow(

                       box(
                         title = "Search results",

                         DT::dataTableOutput("searchResultsTable"),

                         width = 6
                       ),

                       box(
                         title = "History",

                         DT::dataTableOutput("historyTable"),

                         width = 6
                       )
                     )
                   )
          ),

          ## Recommendations and related profile
          tabItem( tabName = "Recommendations",
                   fluidRow(

                     h2( "Recommendations" ),

                     # box(
                     #   title = "Sliders",
                     #
                     #   div( uiOutput( "multiSliders" ), style = "font-size: 90%"),
                     #
                     #   width = 3
                     # ),

                     box(
                       title = "Recommendations (based on history)",

                       numericInput( inputId = "nrecs", label = "Number of Recommendations", min = 1, max = 900, step = 1, value = 12 ),

                       DT::dataTableOutput("recommendationsTable"),

                       width = 6
                     ),

                     box(
                       title = "Profile (based on history)",

                       DT::dataTableOutput("historyProfileTable"),

                       width = 6
                     )
                   )
          ),


          ## Statistics
          tabItem( tabName = "Statistics",
                   fluidRow(

                     h2( "Statistics" ),

                     box(
                       title = "Query",

                       textInput( inputId = "smrPropertyQuery", label = "Properties query:", value = "show the tag types" ),

                       verbatimTextOutput( "propertiesQueryResult" )
                     )

                   )
          )



        )
      )
    )
  )
}


##===========================================================
## Server function
##===========================================================

#' SMRMon interface server function
#' @description Creates the Shiny server function for a sparse matrix recommender interface.
#' @param smrObj A time series recommender.
#' @param itemData A data frame with rows corresponding to items (to be recommended.)
#' @param itemDataColNames Which column names of \code{itemData} should be used in
#' the table displays of search and recommendation results.
#' @param itemDataIDColName Which column of \code{itemData} is with item ID's;
#' those ID's are also row names of \code{smrObj$M}.
#' @param searchColName Which column should be used to search \code{itemData}.
#' @param itemListIDsSplitPattern  A split pattern for the separator of the items list
#' and ratings list.
#' @details  The default value of \code{itemListIDsSplitPattern} is '\\W', but "," should be
#' used if the row ID's have white spaces in them.
#' @return Shiny server function.
#' @family SMR interface functions
#' @export
SMRMonMakeServerFunction <- function( smrObj, itemData, itemDataColNames = NULL, itemDataIDColName = NULL, searchColName = NULL, itemListIDsSplitPattern = "\\W" ) {

  if( is.null(itemDataColNames) ) {
    itemDataColNames <- colnames(itemData)
  }

  if( mean( itemDataColNames %in% colnames(itemData) ) < 1 ) {
    stop( "Not all elements of the argument itemDataColNames are column names of itemData.")
  }

  if( is.null(itemDataIDColName) ) {
    itemDataIDColName <- colnames(itemData)[[1]]
  }

  if( !( itemDataIDColName %in% colnames(itemData) ) ) {
    stop( "The argument itemDataIDColName is not a column name in itemData.")
  }

  if( is.null(searchColName) ) {
    searchColName <- colnames(itemData)[[1]]
  }

  if( !( searchColName %in% colnames(itemData) ) ) {
    stop( "The argument searchColName is not a column name in itemData.")
  }

  # searchColName <- "title"
  # itemDataIDColName <- "id"
  # itemDataColNames <- c("id", "title", "year", "rated", "imdb_rating" )

  if( is.null("itemListIDsSplitPattern") ) {
    itemListIDsSplitPattern <- "\\W"
  }

  shinyServer( function(input, output, session) {

    ##------------------------------------------------------------
    ## SMRMon object onboarding
    ##------------------------------------------------------------

    smrDataLongForm <- reactive(
      smrObj %>% SMRMonGetLongFormData( tagTypesQ = TRUE ) %>% SMRMonTakeValue
    )

    output$smrSummary <- renderPrint({
      summary( as.data.frame( unclass( SMRSparseMatrixToTriplets( smrObj %>% SMRMonTakeM ) ) ) )
    })

    output$tagTypeRangesTable <-
      DT::renderDataTable({ datatable({
        smrObj %>% SMRMonGetProperty( "TagTypeRanges" ) %>% SMRMonTakeValue
      }, rownames = TRUE, filter = 'none', options = list(pageLength = 12, autoWidth = TRUE) ) })

    output$itemDataSummary <- renderPrint({
      summary( as.data.frame( unclass( itemData ) ) )
    })


    ##------------------------------------------------------------
    ## Search and history
    ##------------------------------------------------------------

    ## Probably not optimal...
    searchResultsLongForm <- reactive({

      rowInds <- grep( input$searchString, rownames(smrObj %>% SMRMonTakeM) )

      if( length( rowInds ) > 0 ) {

        smrObj %>%
          SMRMonSetM( (smrObj %>% SMRMonTakeM)[rowInds, ] ) %>%
          SMRMonGetLongFormData( tagTypesQ = TRUE ) %>%
          SMRMonTakeValue

      } else {

        smrObj %>%
          SMRMonFilterMatrix( profile = grep( input$searchString, colnames(smrObj %>% SMRMonTakeM), value = T ) ) %>%
          SMRMonGetLongFormData( tagTypesQ = TRUE ) %>%
          SMRMonTakeValue
      }

    })

    ## OUTPUT: search results table
    output$searchResultsTable <-
      DT::renderDataTable({ datatable({

        searchResultsLongForm()

      }, rownames = TRUE, filter = 'none', options = list(pageLength = 12, autoWidth = TRUE) ) })


    itemListIDs <- reactive({

      ss <- strsplit( input$itemList, split = ",", fixed = FALSE )[[1]]
      ss <- gsub("^[[:space:]]", "", ss)
      ss[ nchar(ss) > 0 ]

    })

    itemListRatings <- reactive({

      res <- strsplit( x = input$itemRatings, split = ",", fixed = FALSE )[[1]]

      res <- as.numeric( res[ nchar(res) > 0 ] )

      if ( length(res) < length( itemListIDs() ) ) {
        res <- c( res, rep(3, length( itemListIDs() ) - length(res) ) )
      } else if ( length(res) > length( itemListIDs() ) ) {
        res <- res[1:length( itemListIDs() )]
      }

      res

    })

    itemListInds <- reactive({
      which( itemData[[ itemDataIDColName ]] %in% itemListIDs() )
      # pmatch( itemListIDs(), itemData[[ itemDataIDColName ]] )
    })

    mHist <- reactive({

      setNames(
        data.frame( Rating = itemListRatings(),
                    ItemID = as.character( itemListIDs() ),
                    stringsAsFactors = FALSE),
        c( "Rating", smrObj %>% SMRMonTakeItemColumnName ) )

    })

    ## OUTPUT: History table
    output$historyTable <-
      DT::renderDataTable({ datatable({

        # mHist() %>%
        #   dplyr::inner_join( smrDataLongForm(), by = smrObj %>% SMRMonTakeItemColumnName )

        mHist() %>%
          dplyr::inner_join( itemData, by = smrObj %>% SMRMonTakeItemColumnName )

      }, rownames = TRUE, filter = 'none', options = list(pageLength = 12, autoWidth = TRUE) ) })


    ##------------------------------------------------------------
    ## Recommendations
    ##------------------------------------------------------------

    output$multiSliders <- renderUI({

      tagTypes <- smrObj %>% SMRMonTakeTagTypes

      purrr::map( tagTypes, function(tt) {
        sliderInput( inputId = paste0('slider.', tt ),  label = tt,  min = 0, max = 10, value = 1 )
      })

    })

    slidersValues <- reactive({

      sliderNames <- paste0( "slider.", smrObj %>% SMRMonTakeTagTypes )
      purrr::map_dbl( setNames( sliderNames, smrObj %>% SMRMonTakeTagTypes ), function(x) input[[x]] )

    })

    recommendations <- reactive({

      if( sum(grepl( "slider", names(input) )) >= length(smrObj %>% SMRMonTakeTagTypes) ) {

        smrObj %>%
          SMRMonApplyTagTypeWeights( weights = slidersValues(), default = 0 ) %>%
          SMRMonRecommend( history = mHist(), nrecs = input$nrecs ) %>%
          SMRMonTakeValue

      } else {

        smrObj %>%
          SMRMonRecommend( history = mHist(), nrecs = input$nrecs ) %>%
          SMRMonTakeValue
      }

    })

    ## OUTPUT: recommendations table
    output$recommendationsTable <-
      DT::renderDataTable({ datatable({

        recommendations() %>%
          dplyr::inner_join( itemData, by = smrObj %>% SMRMonTakeItemColumnName )

      }, rownames = TRUE, filter = 'none', options = list(pageLength = 12, autoWidth = TRUE) ) })

    historyProfile <- reactive({

      if( sum(grepl( "slider", names(input) )) >= length(smrObj %>% SMRMonTakeTagTypes) ) {

        smrObj %>%
          SMRMonApplyTagTypeWeights( weights = slidersValues(), default = 0 ) %>%
          SMRMonProfile( history = mHist() ) %>%
          SMRMonTakeValue

      } else {

        smrObj %>%
          SMRMonProfile( history = mHist() ) %>%
          SMRMonTakeValue

      }

    })

    ## OUTPUT: history profile table
    output$historyProfileTable <-
      DT::renderDataTable({ datatable({

        historyProfile()

      }, rownames = TRUE, filter = 'none', options = list(pageLength = 12, autoWidth = TRUE) ) })


    ##------------------------------------------------------------
    ## Statistics
    ##------------------------------------------------------------

    smrObject3 <- reactive({
      eval( to_SMRMon_R_command( paste0( "use recommender ", input$smrObjName, "; ",  input$smrPropertyQuery ) ) )
    })

    ## OUTPUT: properties query result
    output$propertiesQueryResult <-
      renderPrint({

        smrObject3() %>% SMRMonTakeValue

      })

  })

}


##===========================================================
## Make shiny app
##===========================================================

#' Creation of a SMRMon interface
#' @param smarObj A SMRMon object.
#' @param itemData A data frame with rows corresponding to items (to be recommended.)
#' @param itemDataColNames Which column names of \code{itemData} should be used in
#' the search and recommendation results.
#' @param itemDataIDColName Which column of \code{itemData} is with item ID's;
#' those ID's are also row names of \code{smrObj$M}.
#' @param itemListIDsSplitPattern  A split pattern for the separator of the items list
#' and ratings list.
#' @param dashboardTheme A string specifying the dashbaard theme.
#' See \code{\link{dashboardthemes::shinyDashboardThemes}}.
#' Ignored if the package \code{dashboardthemes} is not installed.
#' @details  The default value of \code{itemListIDsSplitPattern} is '\\W', but "," should be
#' used if the row ID's have white spaces in them.
#' @return Shiny app
#' @family SMRMon interface functions
#' @export
SMRMonCreateSearchInterface <- function( smrObj, itemData, itemDataColNames = NULL, itemDataIDColName = NULL, searchColName = NULL, itemListIDsSplitPattern = "\\W", dashboardTheme = NULL ) {

  res <- length(unlist(strsplit( x = rownames(smrObj$M), split = itemListIDsSplitPattern, fixed = FALSE )))

  if( res != nrow(smrObj$M) ) {
    stop( "The argument itemListIDsSplitPattern splits the rownames of smrObj$M.", call. = TRUE )
  }

  shiny::shinyApp( ui = SMRMonMakeUI( smrObj = smrObj, dashboardTheme = dashboardTheme ),
                   server = SMRMonMakeServerFunction( smrObj = smrObj,
                                                   itemData = itemData,
                                                   itemDataColNames = itemDataColNames,
                                                   itemDataIDColName = itemDataIDColName,
                                                   searchColName = searchColName,
                                                   itemListIDsSplitPattern = itemListIDsSplitPattern )
  )

}
