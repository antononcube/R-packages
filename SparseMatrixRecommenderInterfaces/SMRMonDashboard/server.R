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

    smat <-
      smrObject2() %>%
      SMRMonFilterMatrix( profile = grep( input$searchString, colnames(smrObject2() %>% SMRMonTakeM), value = T ) ) %>%
      SMRMonTakeM

    smatColNames <-  c( smrObject2() %>% SMRMonTakeItemColumnName, "Tag", "Value" )

    res <-
      purrr::map_df( smrObject2() %>% SMRMonTakeTagTypes, function(tt) {
        m <- SMRSubMatrixOfMatrix( smat, smrObject2() %>% SMRMonTakeTagTypeRanges, tt )
        if( is.null(m) || nrow(m) == 0 ) {
          NULL
        } else {
          m <- setNames( SMRSparseMatrixToTriplets( m ), smatColNames )
          cbind( m, TagType = tt, stringsAsFactors = FALSE )
        }
      })

    smatColNames <- c( smrObject2() %>% SMRMonTakeItemColumnName, "TagType", "Tag", "Value" )

    res %>%
      dplyr::select_at( .vars = smatColNames ) %>%
      dplyr::arrange_at( .vars = smatColNames )

  })

  output$searchResultsTable <-
    DT::renderDataTable({ datatable({
      searchResultsLongForm()
    }, rownames = TRUE, filter = 'none', options = list(pageLength = 12, autoWidth = TRUE) ) })


  ##------------------------------------------------------------
  ## Recommendations
  ##------------------------------------------------------------

  output$multiSliders <- renderUI({

    tagTypes <- smrObject2() %>% SMRMonTakeTagTypes

    sliders <-
      purrr::map( tagTypes, function(tt) {
        sliderInput( inputId = paste0('slider.', tt ),  label = tt,  min = 0, max = 10, value = 1 )
      })

  })


  ##------------------------------------------------------------
  ## Statistics
  ##------------------------------------------------------------

  smrObject3 <- reactive({
    eval( to_SMRMon_R_command( paste0( "use recommender ", input$smrObjName, "; ",  input$smrPropertyQuery ) ) )
  })

  output$propertiesQueryResult <-
    renderPrint({

      smrObject3() %>% SMRMonTakeValue

    })



}
