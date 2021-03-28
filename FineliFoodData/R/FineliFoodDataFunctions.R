##===========================================================
## Fineli Food Data package in R
##
## BSD 3-Clause License
##
## Copyright (c) 2020, Anton Antonov
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
##
## * Redistributions of source code must retain the above copyright notice, this
## list of conditions and the following disclaimer.
##
## * Redistributions in binary form must reproduce the above copyright notice,
## this list of conditions and the following disclaimer in the documentation
## and/or other materials provided with the distribution.
##
## * Neither the name of the copyright holder nor the names of its
## contributors may be used to endorse or promote products derived from
## this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
## IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
## FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
## DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
## SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
## CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
## OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
## OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
## Written by Anton Antonov,
## antononcube @@@ posteo ... net,
## Windermere, Florida, USA.
##===========================================================


#' @import dplyr
#' @import magrittr
#' @import purrr
NULL


##===========================================================
## SummarizeTables
##===========================================================

#' Summarize tables
#' @description Summarizes a list of tables.
#' @param tables A list of tables (data frame).
#' If NULL then the Fineli Food Data list of tables \code{lsFineliFoodDataTables} is used.
#' @return A data frame
#' @export
SummarizeTables <- function( tables = NULL ) {

  if( is.null(tables) ) {
    tables = lsFineliFoodDataTables
  }

  if( !is.list(tables) ) {
    stop("The first argument is expected to be a list of data frames.")
  }

  if( is.null(names(tables)) ) {
    names(tables) <- as.character(1:lenght(tables))
  }

  dfFineliTableStats <-
    purrr::map_df( names(tables), function(x) {
      if( !is.data.frame(tables[[x]]) ) {
        stop("The first argument is expected to be a list of data frames.")
      }

      data.frame( Name = x, NRow = nrow(tables[[x]]), NCol = ncol(tables[[x]]), LanguageSpecificQ = "LANG" %in% colnames(tables[[x]]) )
    })

  dfFineliTableStats <- dfFineliTableStats %>% dplyr::arrange(LanguageSpecificQ, desc(NRow))

  dfFineliTableStats
}


##===========================================================
## MakeDictionary
##===========================================================

#' Make dictionary
#' @description Makes an (interpretation) dictionary
#' for a specified table, columns, and language.
#' @param table A data frame or a string of Fineli Food Data table name.
#' (Generic names are allowed, without indication of language.)
#' @param nameFrom A string.
#' If NULL the first integer column is used.
#' @param nameTo A string.
#' If NULL the first character column is used.
#' @param lang Language for the interpretations.
#' One of \code{c("EN", "English", "FI", "Finnish", "SV", "Swedish")}.
#' If NULL then it is the same as "English".
#' @param columnNamePrefixesQ Should the column names be used as prefixes or not?
#' @param sep Separator for the prefixes of the columns names.
#' @return A list with named elements.
#' @export
MakeDictionary <- function( table, nameFrom = NULL, nameTo = NULL, lang = "English", columnNamePrefixesQ = FALSE, sep = ":" ) {

  if( is.null(lang) ) { lang <- "English" }

  if( !( is.character(lang) && length(lang) == 1 ) ) {
    stop("The argument lang is expected to be NULL or a string.", call. = TRUE )
  }

  lsLangMap <- c("EN" = "EN", "English" = "EN", "FI" = "FI", "Finnish" = "FI", "SV" = "SV", "Swedish" = "SV")
  lsExpectedLang <- names(lsLangMap)
  names(lsLangMap) <- tolower(names(lsLangMap))

  if( !( is.character(lang) && length(lang) == 1 && tolower(lang) %in% names(lsLangMap) ) ) {
    stop( paste0( "The argument lang is expected to be NULL or a string, one of '", paste(lsExpectedLang, collapse = "', '"), "'."), call. = TRUE )
  }

  lang <- lsLangMap[[tolower(lang)]]

  if( is.character(table) && length(table) == 1 ) {

    if( table %in% names(lsFineliFoodDataTables) ) {

      table <- lsFineliFoodDataTables[[table]]

    } else if( paste0(table, "_", lang) %in% names(lsFineliFoodDataTables) ) {

      table <- lsFineliFoodDataTables[[paste0(table, "_", lang) ]]
    }

  }

  if( !is.data.frame(table) ) {
    stop( "The first argumment, table, is expected to be a data frame or a string that is a name of Fineli Food Data table.", call. = TRUE )
  }

  if( is.null(nameFrom) ) {
    res <- sapply( table, is.integer)
    if ( sum(res) > 0 ) { nameFrom <- names(res)[res][[1]] }
  }

  if( !(is.character(nameFrom) && nameFrom %in% colnames(table)) ) {
    stop( "The value of nameFrom is not a column of the argument table.", call. = TRUE )
  }


  if( is.null(nameTo) ) {
    res <- sapply( table, is.character)
    if ( sum(res) > 0 ) { nameTo <- names(res)[res][[1]] }
  }

  if( !(is.character(nameTo) && nameTo %in% colnames(table)) ) {
    stop( "The value of nameTo is not a column of the argument table.", call. = TRUE )
  }

  if( columnNamePrefixesQ ) {
    setNames( paste0( nameTo, sep, table[[nameTo]]), paste0( nameFrom, sep, table[[nameFrom]]) )
  } else {
    setNames( table[[nameTo]], table[[nameFrom]])
  }
}
