##=======================================================================================
## General SMRMon Search Interface
## Copyright (C) 2019 Anton Antonov
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
#' @import dplyr
#' @import Matrix
#' @import purrr
#' @import stringr
#' @import shiny
#' @import shinydashboard
#' @import ggplot2
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
SMRMonMakeSearchUI <- function( smrObj, dashboardTheme = NULL ) {


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
          menuItem( "Search by profile", tabName = "SearchByProfile", icon = icon("search-plus") ),
          menuItem( "Collage", tabName = "Collage", icon = icon("object-group") ),
          hr(),
          menuItem( "Sliders", tabName = "Sliders", icon = icon("sliders-h"),
                    uiOutput( "multiSliders" )
          ),
          hr(),
          menuItem( "Facets", tabName = "Facets", icon = icon("boxes"),
                    checkboxGroupInput( inputId = "selectedFacets",
                                        label = "Select facets:",
                                        choices = c( "Profile" = "profile",
                                                     "Tags summary table" = "tagsSummaryTable",
                                                     "Tags summary pie chart" = "tagsSummaryPieChart",
                                                     "Nearest neighbors" = "nearestNeighbors" ),
                                        selected = c( "profile", "nearestNeighbors")
                    )
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


          ## Search/recommendations by profile
          tabItem( tabName = "SearchByProfile",

                   fluidRow(

                     h2( "Search by profile" ),

                     box(
                       title = "Search profile",

                       textInput( inputId = "searchProfileString", label = "Search string:",
                                  value = ".*",
                                  placeholder =  "string pattern"),

                       checkboxInput( inputId = "searchPatternFixed", label = ":fixed search pattern", value = FALSE ),

                       width = 9
                     )

                   ),

                   fluidRow(

                     box(
                       title = "Recommendations",

                       numericInput( inputId = "nrecs", label = "Number of Recommendations", min = 1, max = 900, step = 1, value = 12 ),

                       DT::dataTableOutput("recommendationsTable"),

                       width = 8
                     ),

                     box(
                       title = "Selection profile",

                       DT::dataTableOutput("recommendationsSelectionProfileTable"),

                       width = 4
                     )

                   )
          ),


          ## Facets
          tabItem( tabName = "Collage",
                   fluidRow(

                     h2( "Collage and blending" ),

                     box(
                       title = "Item ID",

                       textInput( inputId = "collageItem", label = "Item:", value = "id.10" ),

                       width = 3
                     ),

                     uiOutput( "collageBoxes" )

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

#' SMRMon search interface server function
#' @description Creates the Shiny server function for a sparse matrix recommender interface.
#' @param smrObj A time series recommender.
#' @param itemData A data frame with rows corresponding to items (to be recommended.)
#' @param itemDataColNames Which column names of \code{itemData} should be used in
#' the table displays of search and recommendation results.
#' @param itemDataIDColName Which column of \code{itemData} is with item ID's;
#' those ID's are also row names of \code{smrObj$M}.
#' @param itemListIDsSplitPattern  A split pattern for the separator of the items list
#' and ratings list.
#' @details  The default value of \code{itemListIDsSplitPattern} is '\\W', but "," should be
#' used if the row ID's have white spaces in them.
#' @return Shiny server function.
#' @family SMRMon interface functions
#' @export
SMRMonMakeSearchServerFunction <- function( smrObj, itemData, itemDataColNames = NULL, itemDataIDColName = NULL, itemListIDsSplitPattern = "\\W" ) {

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

  if( is.null("itemListIDsSplitPattern") ) {
    itemListIDsSplitPattern <- "\\W"
  }

  shinyServer( function(input, output, session) {

    ##------------------------------------------------------------
    ## SMRMon object onboarding
    ##------------------------------------------------------------

    # smrDataLongForm <- reactive(
    #   smrObj %>% SMRMonGetLongFormData( tagTypesQ = TRUE ) %>% SMRMonTakeValue
    # )

    output$smrSummary <- renderPrint({
      smat <- smrObj %>% SMRMonTakeM; smat@x[ smat@x > 0 ] <- 1;
      smat <- (smrObj %>% SMRMonTakeM)[ , order( -colSums(smat) )[1:20] ]
      summary( setNames( as.data.frame( unclass( SMRSparseMatrixToTriplets( smat ) ) )[, 2:3], c( "Tag", "Weight" ) ) )
    })

    output$tagTypeRangesTable <-
      DT::renderDataTable({ datatable({
        smrObj %>% SMRMonGetProperty( "TagTypeRanges" ) %>% SMRMonTakeValue
      }, rownames = TRUE, filter = 'none', options = list(pageLength = 12, autoWidth = TRUE) ) })

    output$itemDataSummary <- renderPrint({
      ##summary( as.data.frame( unclass( itemData ) ) )
      dplyr::glimpse( itemData )
    })


    ##------------------------------------------------------------
    ## Search/recommendations by profile
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

    searchProfile <- reactive({

      sTags <- strsplit( x = input$searchProfileString, split = itemListIDsSplitPattern )[[1]]

      if( input$searchPatternFixed ) {
        ## Fixed search pattern.
        predInds <- sTags %in% colnames( smrObj %>% SMRMonTakeM )

        if( length(predInds) > 0 ) {
          sTags <- sTags[ predInds ]
        } else {
          sTags <- c()
        }

      } else {
        ## General grep pattern.
        sTags <- Reduce( function(a,x) c( a, grep( x, colnames( smrObj %>% SMRMonTakeM ), value = T, ignore.case = T )), init = c(), x = sTags )

      }

      sTags

    })


    recommendations <- reactive({

      localSearchProfile <- searchProfile()

      if( length(localSearchProfile) == 0 ) {

        NULL

      } else if( sum(grepl( "slider", names(input) )) >= length(smrObj %>% SMRMonTakeTagTypes) ) {

        smrObj %>%
          SMRMonApplyTagTypeWeights( weights = slidersValues(), default = 0 ) %>%
          SMRMonRecommendByProfile( profile = localSearchProfile, nrecs = input$nrecs ) %>%
          SMRMonTakeValue

      } else {

        smrObj %>%
          SMRMonRecommendByProfile( profile = localSearchProfile, nrecs = input$nrecs ) %>%
          SMRMonTakeValue
      }

    })

    ## OUTPUT: recommendations table
    output$recommendationsTable <-
      DT::renderDataTable({ datatable({
        if( is.null(recommendations()) ) {
          NULL
        } else {
          recommendations() %>%
            dplyr::inner_join( itemData, by = smrObj %>% SMRMonTakeItemColumnName )
        }
      }, rownames = FALSE, filter = 'top', options = list(pageLength = 12, autoWidth = FALSE), selection = list( mode = 'multiple', selected = c(1)) ) })


    ## OUTPUT: history profile table
    output$recommendationsSelectionProfileTable <-
      output$uproof <- DT::renderDataTable({ datatable({

        sRow <- input$recommendationsTable_rows_selected

        if( is.null(recommendations()) || is.null(sRow) ) {

          NULL

        } else {

          itemID <- recommendations()[sRow, smrObj %>% SMRMonTakeItemColumnName ]

          if( sum(grepl( "slider", names(input) )) >= length(smrObj %>% SMRMonTakeTagTypes) ) {

            smrObj %>%
              SMRMonApplyTagTypeWeights( weights = slidersValues(), default = 0 ) %>%
              SMRMonProfile( history = itemID ) %>%
              SMRMonTakeValue

          } else {

            smrObj %>%
              SMRMonProfile( history = itemID ) %>%
              SMRMonTakeValue

          }

        }

      }, rownames = FALSE, filter = 'top', options = list(pageLength = 12, autoWidth = FALSE) ) })


    ##------------------------------------------------------------
    ## Facets
    ##------------------------------------------------------------

    collageItemSummary <- reactive({

      smrObj %>%
        SMRMonSummarizeItem( item = input$collageItem, tagTypesQ = TRUE, nTopTags = 12 ) %>%
        SMRMonTakeValue

    })

    collageItemNearestNeigbors <- reactive({

      if( sum(grepl( "slider", names(input) )) >= length(smrObj %>% SMRMonTakeTagTypes) ) {

        smrObj %>%
          SMRMonApplyTagTypeWeights( weights = slidersValues(), default = 0 ) %>%
          SMRMonRecommend( history = input$collageItem, nrecs = 12 ) %>%
          SMRMonJoinAcross( data = itemData ) %>%
          SMRMonTakeValue

      } else {

        smrObj %>%
          SMRMonRecommend( history = input$collageItem, nrecs = 12 ) %>%
          SMRMonJoinAcross( data = itemData ) %>%
          SMRMonTakeValue

      }

    })


    output$collageBoxes <- renderUI({

      if( !is.list( collageItemSummary() ) ) {

        NULL

      } else {

        purrr::map( input$selectedFacets, function(sf) {

          if( sf == "profile" ) {

            box( title = "Profile",

                 ## renderTable( collageItemSummary()$Profile ),
                 DT::renderDataTable({ datatable({

                   collageItemSummary()$Profile

                 }, rownames = FALSE, filter = 'none', options = list(pageLength = 8, autoWidth = TRUE) ) }),

                 width = 4 )

          } else if ( sf == "tagsSummaryTable" ) {

            box( title = "Tags summary",

                 ##renderTable( collageItemSummary()$TagsSummary ),
                 DT::renderDataTable({ datatable({

                   collageItemSummary()$TagsSummary

                 }, rownames = FALSE, filter = 'none', options = list(pageLength = 8, autoWidth = TRUE) ) }),

                 width = 4 )

          } else if ( sf == "tagsSummaryPieChart" ) {

            box( title = "Tags summary pie chart",

                 ## renderPlot( pie( collageItemSummary()$TagsSummary$NumberOfTags ) ),

                 renderPlot(

                   ggplot( collageItemSummary()$TagsSummary, aes( x = "", y = NumberOfTags, fill = TagType) ) +
                     geom_bar(width = 1, stat = "identity") +
                     coord_polar( "y", start = 0 )

                 ),

                 width = 4 )

          } else if ( sf == "nearestNeighbors" ) {

            box( title  = "Nearest neighbors",

                 ##renderTable( collageItemNearestNeigbors() ),
                 DT::renderDataTable({ datatable({

                   collageItemNearestNeigbors()

                 }, rownames = FALSE, filter = 'none', options = list(pageLength = 8, autoWidth = TRUE) ) }),

                 width = 12 )

          } else {

            NULL
          }
        })
      }

    })

  })

}


##===========================================================
## Make shiny app
##===========================================================

#' Creation of a SMRMon search interface
#' @param smrObj A SMRMon object.
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
SMRMonCreateSearchInteractiveInterface <- function( smrObj, itemData, itemDataColNames = NULL, itemDataIDColName = NULL, itemListIDsSplitPattern = "\\W", dashboardTheme = NULL ) {

  res <- length(unlist(strsplit( x = rownames(smrObj$M), split = itemListIDsSplitPattern, fixed = FALSE )))

  if( res != nrow(smrObj$M) ) {
    stop( "The argument itemListIDsSplitPattern splits the rownames of smrObj$M.", call. = TRUE )
  }

  shiny::shinyApp( ui = SMRMonMakeSearchUI( smrObj = smrObj, dashboardTheme = dashboardTheme ),
                   server = SMRMonMakeSearchServerFunction( smrObj = smrObj,
                                                            itemData = itemData,
                                                            itemDataColNames = itemDataColNames,
                                                            itemDataIDColName = itemDataIDColName,
                                                            itemListIDsSplitPattern = itemListIDsSplitPattern )
  )

}



