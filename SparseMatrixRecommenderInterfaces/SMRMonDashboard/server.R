##=======================================================================================
## General Sparse Matrix Recommender Dashboard - server part
## Copyright (C) 2019  Anton Antonov
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

library(shiny)
library(shinydashboard)
library(DT)

function(input, output) {

  ##------------------------------------------------------------
  ## SMRMon object onboarding
  ##------------------------------------------------------------

  smrObject2 <- reactive(
    get(input$smrObjName)
  )

  # dfWideForm <- reactive(
  #   get(input$dfWideFormName)
  # )

  smrDataLongForm <- reactive(
    smrObject2() %>% SMRMonGetLongFormData( tagTypesQ = TRUE ) %>% SMRMonTakeValue
  )

  output$smrSummary <- renderPrint({
    summary( as.data.frame( unclass( SMRSparseMatrixToTriplets( smrObject2() %>% SMRMonTakeM ) ) ) )
  })

  output$tagTypeRangesTable <-
    DT::renderDataTable({ datatable({
      smrObject2() %>% SMRMonGetProperty( "TagTypeRanges" ) %>% SMRMonTakeValue
    }, rownames = TRUE, filter = 'none', options = list(pageLength = 12, autoWidth = TRUE) ) })


  ##------------------------------------------------------------
  ## Search and history
  ##------------------------------------------------------------

  ## Probably not optimal...
  searchResultsLongForm <- reactive({

    rowInds <- grep( input$searchString, rownames(smrObject2() %>% SMRMonTakeM) )

    if( length( rowInds ) > 0 ) {

      smrObject2() %>%
        SMRMonSetM( (smrObject2() %>% SMRMonTakeM)[rowInds, ] ) %>%
        SMRMonGetLongFormData( tagTypesQ = TRUE ) %>%
        SMRMonTakeValue

    } else {

      smrObject2() %>%
        SMRMonFilterMatrix( profile = grep( input$searchString, colnames(smrObject2() %>% SMRMonTakeM), value = T ) ) %>%
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
      c( "Rating", smrObject2() %>% SMRMonTakeItemColumnName ) )
  })

  ## OUTPUT: History table
  output$historyTable <-
    DT::renderDataTable({ datatable({

      mHist() %>%
        dplyr::inner_join( smrDataLongForm(), by = smrObject2() %>% SMRMonTakeItemColumnName )

      # mHist() %>%
      #   dplyr::inner_join( dfWideForm(), by = smrObject2() %>% SMRMonTakeItemColumnName )

    }, rownames = TRUE, filter = 'none', options = list(pageLength = 12, autoWidth = TRUE) ) })


  ##------------------------------------------------------------
  ## Recommendations
  ##------------------------------------------------------------

  output$multiSliders <- renderUI({

    tagTypes <- smrObject2() %>% SMRMonTakeTagTypes

    purrr::map( tagTypes, function(tt) {
      sliderInput( inputId = paste0('slider.', tt ),  label = tt,  min = 0, max = 10, value = 1 )
    })

  })

  slidersValues <- reactive({

    sliderNames <- paste0( "slider.", smrObject2() %>% SMRMonTakeTagTypes )
    purrr::map_dbl( setNames( sliderNames, smrObject2() %>% SMRMonTakeTagTypes ), function(x) input[[x]] )

  })

  recommendations <- reactive({

    smrObject2() %>%
      SMRMonApplyTagTypeWeights( weights = slidersValues(), default = 0 ) %>%
      SMRMonRecommend( history = mHist(), nrecs = input$nrecs ) %>%
      SMRMonTakeValue

  })

  ## OUTPUT: recommendations table
  output$recommendationsTable <-
    DT::renderDataTable({ datatable({

      recommendations() %>%
        dplyr::inner_join( dfWideForm(), by = smrObject2() %>% SMRMonTakeItemColumnName )

    }, rownames = TRUE, filter = 'none', options = list(pageLength = 12, autoWidth = TRUE) ) })

  historyProfile <- reactive({

    smrObject2() %>%
      SMRMonApplyTagTypeWeights( weights = slidersValues(), default = 0 ) %>%
      SMRMonProfile( history = mHist() ) %>%
      SMRMonTakeValue

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



}
