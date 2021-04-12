##===========================================================
## Random Data Frame Generator in R
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
## SplitVariableValues
##===========================================================

#' Split variable values
#' @description For a given data frame splits the values of specified variables.
#' It is assumed that the given data frame has ID, Variable, and Value columns.
#' (I.e. the data frame is in long form.)
#' @param data A data frame.
#' @param variableToSplit A character vector.
#' @param splitPattern Split pattern
#' @param idColumn Identifier column.
#' @param variableColumn Name of the column that has the variables.
#' @param valueColumn Name of the column that has the values.
#' @return A data frame.
#' @export
SplitVariableValues <- function( data, variableToSplit, splitPattern = ";", idColumn = "ID", variableColumn = "Variable", valueColumn = "Value" ) {

  if( !is.data.frame(data) ) {
    stop("The argument data is expected to be a data frame.", call. = TRUE)
  }

  if( !is.character(variableToSplit) ) {
    stop("The argument variableToSplit is expected to be a character vector.", call. = TRUE)
  }

  if( !( is.character(splitPattern) && length(sep) == 1 ) ) {
    stop("The argument splitPattern is expected to be a string.", call. = TRUE)
  }

  if( !( is.character(idColumn) && length(idColumn) == 1 && idColumn %in% colnames(data) ) ) {
    stop("The argument idColumn is expected to be a string that is a column name of the argument data.", call. = TRUE)
  }

  if( !( is.character(variableColumn) && length(variableColumn) == 1 && variableColumn %in% colnames(data) ) ) {
    stop("The argument variableColumn is expected to be a string that is a column name of the argument data.", call. = TRUE)
  }

  if( !( is.character(valueColumn) && length(valueColumn) == 1 && valueColumn %in% colnames(data) ) ) {
    stop("The argument valueColumn is expected to be a string that is a column name of the argument data.", call. = TRUE)
  }

  ## Batch processing of multiple split variables
  if( length(variableToSplit) > 1 ) {

    Reduce(
      f = function(a,x) SplitVariablValues( data = a,
                                            variableToSplit = x,
                                            sep = sep,
                                            idColumn = idColumn,
                                            variableColumn = variableColumn,
                                            valueColumn = valueColumn),
      init = data,
      x = variableToSplit,
    )

  }

  ## Single variable to split
  dfRandLongFormPart <- dfRandLongForm[ dfRandLongForm[[variableColumn]] == variableToSplit, ]

  dfRandLongFormPart <-
    purrr::map_df( split(dfRandLongFormPart, 1:nrow(dfRandLongFormPart) ), function(dfX) {
      res <- data.frame( ID = dfX[[idColumn]], Variable = dfX[[variableColumn]], Value = trimws(strsplit(dfX[[valueColumn]], splitPattern)[[1]]) )
      setNames(res, c(idColumn, variableColumn, valueColumn))
    })

  dfRandLongForm2 <-
    rbind(
      dfRandLongForm[ dfRandLongForm[[variableColumn]] != variableToSplit, ],
      dfRandLongFormPart
    ) %>%
    dplyr::mutate( Weight = 1, Value = tolower(Value) )

  dfRandLongForm2[ order(dfRandLongForm2[[idColumn]]), ]
}
