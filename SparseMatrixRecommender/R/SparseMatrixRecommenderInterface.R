##=======================================================================================
## General Sparse Matrix Recommender Interface
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
## This Shiny interface is made to be used with the recommender system implemented in:
##  https://github.com/antononcube/MathematicaForPrediction/blob/master/R/SparseMatrixRecommender.R
##
## The two central objects are:
## 1. 'itemData' -- a data frame with rows corresponding to items, and
## 2. 'itemSMR' -- a sparse matrix recommender object.
##
## The following variables have to be set:
## 1. searchColName -- which column of itemData is going to be used for searching
##      (e.g. "title" );
## 2. itemDataIDColName -- which column of itemData is with item IDs;
##      these IDs are also rownames of itemSMR$M;
## 3. itemDataColNames -- which column names of itemData should be used in the search
##      and recommendation results.
## 4. itemListIDsSplitPattern -- a split pattern for the separator of the items list
##      and ratings list. By default is "\\W". (But "," should be used if the row IDs
##      have white spaces in them.)
##
##=======================================================================================


#' @import DT
#' @import purrr
#' @import shiny
#' @import shinydashboard
NULL

##===========================================================
## UI function
##===========================================================

#' Sparse matrix recommender interface UI
#' @description Creates the Shiny UI function for a sparse matrix recommender interface. 
#' @param itemSMR A sparse matrix recommender.
#' @return Shiny UI object.
#' @family Time series search interface functions
#' @export
SMRMakeUI <- function( itemSMR ) {
  
  shinyUI(
    fluidPage(
      
      titlePanel("Item recommendations interface"),
      
      fluidRow(
        
        column( 3,
                submitButton( "Update", icon = icon("refresh") ),
                
                textInput("search", "Search items with:", "A.*"),
                
                textInput("itemList", "Item list:", rownames(itemSMR$M)[1] ),
                
                textInput("itemRatings", "Item star ratings:", "3"),
                
                numericInput("nrecs", "Number of recommendations:", 20)
                
        ),
        
        column( 6,
                tabsetPanel( 
                  tabPanel( "Search results", 
                            h4("Search results"),
                            DT::dataTableOutput("view")
                  ),
                  
                  tabPanel( "Consumed items list",
                            h4("Consumed items list"),
                            tabsetPanel(
                              tabPanel( "short", DT::dataTableOutput("itemList") ),
                              tabPanel( "extended", DT::dataTableOutput("itemListData") ) 
                            )
                  )
                )              
        ),
        
        column( 3, 
                textInput("tagTypeSFactors", "Tag type significance factors:", 
                          paste( "c(", paste( SMRCurrentTagTypeSignificanceFactors( itemSMR ), collapse = ", " ), ")" ) ),
                DT::dataTableOutput("significanceFactors")
        )
      ),
      
      fluidRow( 
        column( 9,
                h4("Recommendations"),
                tabsetPanel(
                  tabPanel( "main", DT::dataTableOutput("recs") ),
                  tabPanel( "proofs", DT::dataTableOutput("recsProofs") ),
                  tabPanel( "scores plot", plotOutput("recsScoresPlot") )
                )
        ),
        column( 3,
                tabsetPanel(
                  tabPanel( "Profile",
                            textInput("selectedProfileTags", "Selected tags:", ""),
                            h4("Profile"),
                            DT::dataTableOutput("uprofile") ),
                  tabPanel( "Detailed proof",
                            h4("Detailed proof"),
                            DT::dataTableOutput("uproof") )
                )
        )
      )
    )
  )
  
}


##===========================================================
## Server function
##===========================================================

#' Sparse matrix recommender interface server function
#' @description Creates the Shiny server function for a sparse matrix recommender interface. 
#' @param itemSMR A time series recommender.
#' @param itemData A data frame with rows corresponding to items (to be recommended.)
#' @param itemDataColNames Which column names of \code{itemData} should be used in 
#' the search and recommendation results.
#' @param itemDataIDColName Which column of \code{itemData} is with item ID's;
#' those ID's are also row names of \code{itemSMR$M}.
#' @param itemListIDsSplitPattern  A split pattern for the separator of the items list
#' and ratings list.
#' @details  The default value of \code{itemListIDsSplitPattern} is '\\W', but "," should be 
#' used if the row ID's have white spaces in them.
#' @return Shiny server function.
#' @family SMR interface functions
#' @export
SMRMakeServerFunction <- function( itemSMR, itemData, itemDataColNames = NULL, itemDataIDColName = NULL, searchColName = NULL, itemListIDsSplitPattern = "\\W" ) {
  

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
  
  shinyServer(function(input, output, session) {
    
    tagTypeSFactors <- reactive({
      
      pres <- eval( parse( text= paste( "c(", input$tagTypeSFactors, ")" ) ) )
      if ( is.null( names(pres) ) ) {
        pres <- setNames( c( pres, rep( 0, length(itemSMR$TagTypes) - length(pres) ) ), itemSMR$TagTypes )
      } else {
        default <- if ( is.na( pres["Default"] ) ) { 0 } else { pres["Default"] }
        cnames <- intersect( itemSMR$TagTypes, names(pres) )
        diffnames <- setdiff( itemSMR$TagTypes, cnames )
        pres <- c( pres[cnames], setNames( rep( default, length(diffnames) ), diffnames ) )
        pres <- pres[ itemSMR$TagTypes ]
      }
      pres
    })
    
    
    itemListIDs <- reactive({
      ss <- strsplit( input$itemList, split = itemListIDsSplitPattern, fixed = FALSE )[[1]]
      ss <- gsub("^[[:space:]]", "", ss)
      ss[ nchar(ss) > 0 ]
    })
    
    itemListRatings <- reactive({
      res <- strsplit( x = input$itemRatings, split = itemListIDsSplitPattern, fixed = FALSE )[[1]]
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
        c("Rating", itemSMR$ItemColumnName) )
    })
    
    # selectedProfileTags <- reactive({
    #   as.integer( eval( parse( text = input$selectedProfileTags ) ) )
    # })
    
    selectedProfileTags <- reactive({
      ss <- str_split( input$selectedProfileTags, pattern = ",|\\W")[[1]]
      ss <- ss[ nchar(ss) > 0 ]
      if ( length(ss) == 0 ) { NULL }
      else { as.integer( ss ) }
    })
    
    
    output$significanceFactors <- DT::renderDataTable({ datatable({
      data.frame( TagType = names(tagTypeSFactors()), S.Factor = tagTypeSFactors() )
    }, rownames = FALSE, options = list(pageLength = 8 ) ) })
    
    ## Recommendations
    recommendations <- reactive({
      
      nrecs <- max( 100, input$nrecs )
      itemSMR$M <- SMRApplyTagTypeWeights( itemSMR, tagTypeSFactors()  )

      if( FALSE ) {
        ## This is much simpler for SMR objects:
        res <- SMRRecommendationsDF( itemSMR, mHist(), nrecs )
      } else {
        res <- Recommendations( x = itemSMR, historyItems = mHist()[[2]], historyRatings = mHist()[[1]], nrecs = nrecs )
        ## Because the general S3 function removes the indices we have to add them in order the rest of the code to work
        res <- data.frame( Score = res$Score, Index = pmatch( res$Item, rownames(itemSMR$M), nomatch = 0 ), Item = as.character(res$Item), stringsAsFactors=FALSE)
        names(res) <- c("Score", "Index", itemSMR$ItemColumnName)
      }
      
      if ( length( selectedProfileTags() ) > 0 ) {
        res <- SMRReorderRecommendations( itemSMR, res, selectedProfileTags() )
      }
      res[1:input$nrecs,]
    })
    
    userProfile <- reactive({
      itemSMR$M <- SMRApplyTagTypeWeights( itemSMR, tagTypeSFactors() )
      SMRProfileDF( itemSMR, mHist() )
    })
    
    extendedItemData <- reactive({
      ## Extended data
      tags <- purrr::map_chr( 1:nrow(mHist()), function(i) {
        pdf <- SMRMetadataProofs( smr = itemSMR, toBeLovedItem = mHist()[i,2], profile = userProfile(), normalizeScores = TRUE )
        paste( pdf$Tag, collapse="; ")
      })
      res <- cbind( mHist(), Tags = tags, stringsAsFactors = FALSE )
      res
    })
    
    ## The list of user consumed items
    output$itemList <- DT::renderDataTable({ datatable({
      res <- itemData[ itemListInds(), itemDataColNames ]
      res <- cbind( res, StarRating=itemListRatings() )
      res
    }, rownames = FALSE, filter = 'top', options = list(pageLength = 12, autoWidth = FALSE) ) })
    
    output$itemListData <- DT::renderDataTable({ datatable({
      extendedItemData()
    }, rownames = FALSE, filter = 'top', options = list(pageLength = 12, autoWidth = FALSE) ) })
    
    ## Using simple title search
    output$view <- DT::renderDataTable({ datatable({
      inds <- grep( pattern=input$search, iconv( itemData[[searchColName]] ), ignore.case=TRUE )
      res <- itemData[ inds, itemDataColNames ]
      res
    }, rownames = FALSE, filter = 'top', options = list(pageLength = 4, autoWidth = FALSE) ) })
    
    
    output$recs <-
      DT::renderDataTable({ datatable({
        ## Do extensions of the recommendations with needed
        resIDs <- recommendations()[[3]]
        res <- merge( x = recommendations(), y = itemData[, itemDataColNames ], by.x = itemSMR$ItemColumnName, by.y = itemDataIDColName, all.x = TRUE )
        ## This is done because merge breaks the order.
        res[ match( resIDs, res[[1]] ), ]
      }, rownames = FALSE, filter = 'top', options = list(pageLength = 12, autoWidth = FALSE), selection = list( mode = 'single', selected = c(1)) ) })
    
    output$recsProofs <-
      DT::renderDataTable({ datatable({
        itemSMR$M <- SMRApplyTagTypeWeights( itemSMR, tagTypeSFactors()  )
        proofs <- purrr::map_chr( recommendations()[[3]], function(x) {
          pdf <- SMRMetadataProofs( smr = itemSMR, toBeLovedItem = x, profile = userProfile(), normalizeScores = TRUE )
          if ( length( selectedProfileTags() ) > 0 ) {
            pdf <- pdf[ pdf$Index %in% selectedProfileTags(), ]
          }
          paste( pdf$Tag, collapse="; ")
        })

        ## Recommendations extensions with proofs
        res <- cbind( recommendations(), Proofs=proofs, stringsAsFactors = FALSE )
        res[-2]
      }, rownames = FALSE, filter = 'top', options = list(pageLength = 12, autoWidth = FALSE) ) })
    
    output$recsScoresPlot <- renderPlot({
      plot( recommendations()$Score, main="Recommendation scores", xlab="recommendations row number", ylab="score")
    })
    
    output$uprofile <- DT::renderDataTable({ datatable({
      itemSMR$M <- SMRApplyTagTypeWeights( itemSMR, tagTypeSFactors() )
      res <- SMRProfileDF( itemSMR, mHist() )
      res$Tag <-  iconv( res$Tag )
      cbind( res, TagType = purrr::map_chr( res$Index, function(x) SMRTagType( itemSMR, x ) ) )
    }, rownames = FALSE, filter = 'top', options = list(pageLength = 12, autoWidth = FALSE) ) })
    
    output$uproof <- DT::renderDataTable({ datatable({
      sRow <- input$recs_rows_selected
      if( is.null(sRow) ) { NULL 
      } else { 
        itemID <- recommendations()[sRow,2]
        SMRMetadataProofs( smr = itemSMR, toBeLovedItem = c(itemID), profile = userProfile(), normalizeScores = TRUE)
      }
    }, rownames = FALSE, filter = 'top', options = list(pageLength = 12, autoWidth = FALSE) ) })
    
  })
  
}  

##===========================================================
## Make shiny app
##===========================================================

#' Creation of a sparse matrix recommender interface 
#' @param itemSMR A time series recommender.
#' @param itemData A data frame with rows corresponding to items (to be recommended.)
#' @param itemDataColNames Which column names of \code{itemData} should be used in 
#' the search and recommendation results.
#' @param itemDataIDColName Which column of \code{itemData} is with item ID's;
#' those ID's are also row names of \code{itemSMR$M}.
#' @param itemListIDsSplitPattern  A split pattern for the separator of the items list
#' and ratings list.
#' @details  The default value of \code{itemListIDsSplitPattern} is '\\W', but "," should be 
#' used if the row ID's have white spaces in them.
#' @return Shiny app
#' @return Shiny app object.
#' @family SMR interface functions
#' @export
SMRCreateSearchInterface <- function( itemSMR, itemData, itemDataColNames = NULL, itemDataIDColName = NULL, searchColName = NULL, itemListIDsSplitPattern = "\\W" ) {
  
  res <- length(unlist(strsplit( x = rownames(itemSMR$M), split = itemListIDsSplitPattern, fixed = FALSE )))
  
  if( res != nrow(itemSMR$M) ) {
    stop( "The argument itemListIDsSplitPattern splits the rownames of itemSMR$M.", call. = TRUE )
  }
  
  shiny::shinyApp( ui = SMRMakeUI( itemSMR = itemSMR ), 
                   server = SMRMakeServerFunction( itemSMR = itemSMR, 
                                                   itemData = itemData, 
                                                   itemDataColNames = itemDataColNames,
                                                   itemDataIDColName = itemDataIDColName,
                                                   searchColName = searchColName,
                                                   itemListIDsSplitPattern = itemListIDsSplitPattern )
  )
  
}