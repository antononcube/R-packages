#=======================================================================================
# Sparse matrix recommender monad in R
# Copyright (C) 2019  Anton Antonov
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Written by Anton Antonov,
# antononcube @@@ posteo ... net ,
# Windermere, Florida, USA.
#
#=======================================================================================
#
# This software monad is a "monadic interface" to the package
# SparseMatrixRecommender; see:
#   https://github.com/antononcube/R-packages/tree/master/SparseMatrixRecommender ,
#
# This package is used in the package SparseMatrixRecommenderInterfaces; see:
#   https://github.com/antononcube/R-packages/tree/master/SparseMatrixRecommenderInterfaces .
#
#=======================================================================================


#' @import SparseMatrixRecommender
#' @import OutlierIdentifiers
#' @import Matrix
#' @import magrittr
#' @import purrr
#' @import dplyr
NULL

##===========================================================
## SMRMon failure symbol
##===========================================================

#' Failure symbol for SMRMon.
#' @description Failure symbol for the monad SMRMon.
#' @export
SMRMonFailureSymbol <- NA

#' Failure test for an SMRMon object.
#' @description Test is an SMRMon object a failure symbol.
#' @export
SMRMonFailureQ <- function(x) { mean(is.na(x)) }


##===========================================================
## SMRMon Unit
##===========================================================

#' Make a SMRMon Unit
#' @description Creates a monad object.
#' @return An S3 class "SMR". In other words, a list with the attribute "class" set to "SMR".
#' @export
SMRMonUnit <- function( data = NULL ) {

  res <- list( Value = NULL, M = NULL, M01 = NULL, TagTypeRanges = NULL, TagTypes = NULL, ItemColumnName = NULL, TagToIndexRules = NULL, ItemToIndexRules = NULL, Data = NULL )
  attr(res, "class") <- "SMR"

  res$Data <- data

  res
}


##===========================================================
## Value setter and getter
##===========================================================

#' Set the value in a SMRMon object.
#' @description Sets the value in a SMRMon monad object.
#' @param smrObj An SMRMon object.
#' @param value The new value.
#' @return A SMRMon object.
#' @details Assigns \code{value} to \code{smrObj$Value}.
#' @family Set/Take functions
#' @export
SMRMonSetValue <- function( smrObj, value ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  smrObj$Value <- value
  smrObj
}

#' Take the value in a SMRMon object.
#' @description Takes the value from SMRMon monad object.
#' @param smrObj An SMRMon object.
#' @return Just \code{smrObj$Value}.
#' @family Set/Take functions
#' @export
SMRMonTakeValue <- function( smrObj ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  smrObj$Value
}


##===========================================================
## Member presence check
##===========================================================

#' General member presence check.
#' @description A general function for checking the presence of a data member in an SMRMon object.
#' @param smrObj An SMRMon object.
#' @param memberName The name of the member to be checked.
#' @param memberPrettyName A pretty member name (for messages).
#' @param functionName The name of the delegating function.
#' @param logicalResultQ Should the result be a logical value?
#' @param warningQ Should a warning be issued or not?
#' @return A logical value or an SMRMon object.
#' @export
SMRMonMemberPresenceCheck <- function( smrObj, memberName, memberPrettyName = memberName, functionName = "", logicalResultQ = FALSE, warningQ = TRUE ) {

  if( !is.list(smrObj) ) { return(SMRMonFailureSymbol) }

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  res <- TRUE

  if( nchar(functionName) > 0 ) { functionName <- paste0( functionName, ":: ") }

  if( is.null(smrObj[[memberName]]) ) {
    if( warningQ ) {
      warning( paste0( functionName, paste0("Cannot find ", memberPrettyName, ".") ), call. = TRUE )
    }
    res <- FALSE
  }

  if( logicalResultQ ) { res }
  else if ( !logicalResultQ && !res) { SMRMonFailureSymbol }
  else { smrObj }
}


##===========================================================
## Echo value
##===========================================================

#' Echo monad's value.
#' @description Echoes the monad object value.
#' @param smrObj An SMRMon object.
#' @return An SMRMon object
#' @family Echo functions
#' @export
SMRMonEchoValue <- function( smrObj ) {
  SMRMonEchoFunctionValue( smrObj, function(x) x )
}


##===========================================================
## Echo function application of over monad's value
##===========================================================

#' Function application to monad's value.
#' @description Apply a function to the "Value" element/member of the monad object.
#' @param smrObj An SMRMon object.
#' @param f A function to be applied to \code{smrObj$Value}.
#' @return A SMRMon object.
#' @details Prints \code{f(smrObj$Value)}.
#' @family Echo functions
#' @export
SMRMonEchoFunctionValue <- function( smrObj, f ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  print( f(smrObj$Value) )

  smrObj
}


##===========================================================
## Echo
##===========================================================

#' Echo.
#' @description Echoes the given argument.
#' @param smrObj An SMRMon object.
#' @param x Object to be echoed.
#' @return An SMRMon object
#' @family Echo functions
#' @export
SMRMonEcho <- function( smrObj, x ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  print(x)

  smrObj
}


##===========================================================
## Optional function application over monad's object
##===========================================================

#' Optional function application to the monad object.
#' @description If monadic failure is obtained from \code{f(smrObj)}
#' then returns the original \code{smrObj};
#' else returns the result of \code{f(smrObj)}.
#' @param smrObj An SMRMon object.
#' @param f A function to be applied to the monad object.
#' @return A SMRMon object.
#' @details In general \code{f} should return a monad object,
#' but that is not enforced.
#' @export
SMRMonOption <- function( smrObj, f ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  res <- smrObj %>% f

  if( SMRMonFailureQ(res) ) { return(smrObj) }

  res
}


##===========================================================
## M setter
##===========================================================

#' Set M.
#' @description Sets M into the monad object.
#' @param smrObj An SMRMon object.
#' @param M An object member to be set.
#' @return An SMRMon object.
#' @family Set/Take functions
#' @export
SMRMonSetM <- function( smrObj, M ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !( is.null(M) || "dgCMatrix" %in% class(M) ) ) {
    warning("The argument M is expected to be NULL or a 'dgCMatrix' sparse matrix.", call. = TRUE)
    return(SMRMonFailureSymbol)
  }

  smrObj$M <- M

  smrObj
}

##===========================================================
## M01 setter
##===========================================================

#' Set M01.
#' @description Sets M01 into the monad object.
#' @param smrObj An SMRMon object.
#' @param M01 An object member to be set.
#' @return An SMRMon object.
#' @family Set/Take functions
#' @export
SMRMonSetM01 <- function( smrObj, M01 ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !( is.null(M01) ||  "dgCMatrix" %in% class(M01) ) ) {
    warning("The argument M01 is expected to be NULL or a 'dgCMatrix' sparse matrix", call. = TRUE)
    return(SMRMonFailureSymbol)
  }

  smrObj$M01 <- M01

  smrObj
}

##===========================================================
## TagTypeRanges setter
##===========================================================

#' Set TagTypeRanges.
#' @description Sets TagTypeRanges into the monad object.
#' @param smrObj An SMRMon object.
#' @param TagTypeRanges An object member to be set.
#' @return An SMRMon object.
#' @family Set/Take functions
#' @export
SMRMonSetTagTypeRanges <- function( smrObj, TagTypeRanges ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !( is.null(TagTypeRanges) || is.data.frame(TagTypeRanges)) ) {
    warning("The argument TagTypeRanges is expected to be NULL or a data.frame.", call. = TRUE)
    return(SMRMonFailureSymbol)
  }

  smrObj$TagTypeRanges <- TagTypeRanges

  smrObj
}

##===========================================================
## TagTypes setter
##===========================================================

#' Set TagTypes.
#' @description Sets TagTypes into the monad object.
#' @param smrObj An SMRMon object.
#' @param TagTypes An object member to be set.
#' @return An SMRMon object.
#' @family Set/Take functions
#' @export
SMRMonSetTagTypes <- function( smrObj, TagTypes ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !( is.null(TagTypes) || is.character(TagTypes)) ) {
    warning("The argument TagTypes is expected to be NULL or a character.", call. = TRUE)
    return(SMRMonFailureSymbol)
  }

  smrObj$TagTypes <- TagTypes

  smrObj
}

##===========================================================
## ItemColumnName setter
##===========================================================

#' Set ItemColumnName.
#' @description Sets ItemColumnName into the monad object.
#' @param smrObj An SMRMon object.
#' @param ItemColumnName An object member to be set.
#' @return An SMRMon object.
#' @family Set/Take functions
#' @export
SMRMonSetItemColumnName <- function( smrObj, ItemColumnName ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !( is.null(ItemColumnName) || is.character(ItemColumnName)) ) {
    warning("The argument ItemColumnName is expected to be NULL or a character.", call. = TRUE)
    return(SMRMonFailureSymbol)
  }

  smrObj$ItemColumnName <- ItemColumnName

  smrObj
}

##===========================================================
## TagToIndexRules setter
##===========================================================

#' Set TagToIndexRules.
#' @description Sets TagToIndexRules into the monad object.
#' @param smrObj An SMRMon object.
#' @param TagToIndexRules An object member to be set.
#' @return An SMRMon object.
#' @family Set/Take functions
#' @export
SMRMonSetTagToIndexRules <- function( smrObj, TagToIndexRules ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !( is.null(TagToIndexRules) || is.integer(TagToIndexRules)) ) {
    warning("The argument TagToIndexRules is expected to be NULL or a integer.", call. = TRUE)
    return(SMRMonFailureSymbol)
  }

  smrObj$TagToIndexRules <- TagToIndexRules

  smrObj
}

##===========================================================
## ItemToIndexRules setter
##===========================================================

#' Set ItemToIndexRules.
#' @description Sets ItemToIndexRules into the monad object.
#' @param smrObj An SMRMon object.
#' @param ItemToIndexRules An object member to be set.
#' @return An SMRMon object.
#' @family Set/Take functions
#' @export
SMRMonSetItemToIndexRules <- function( smrObj, ItemToIndexRules ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !( is.null(ItemToIndexRules) || is.integer(ItemToIndexRules)) ) {
    warning("The argument ItemToIndexRules is expected to be NULL or a integer.", call. = TRUE)
    return(SMRMonFailureSymbol)
  }

  smrObj$ItemToIndexRules <- ItemToIndexRules

  smrObj
}

##===========================================================
## Data setter
##===========================================================

#' Set Data.
#' @description Sets Data into the monad object.
#' @param smrObj An SMRMon object.
#' @param Data An object member to be set.
#' @return An SMRMon object.
#' @family Set/Take functions
#' @export
SMRMonSetData <- function( smrObj, Data ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !( is.null(Data) || is.data.frame(Data)) ) {
    warning("The argument Data is expected to be NULL or a data.frame.", call. = TRUE)
    return(SMRMonFailureSymbol)
  }

  smrObj$Data <- Data

  smrObj
}

##===========================================================
## M Taker
##===========================================================

#' Take M.
#' @description Takes M from the monad object.
#' @param smrObj An SMRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{SMRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
SMRMonTakeM <- function( smrObj, functionName = "SMRMonTakeM" ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "M", memberPrettyName = "M", functionName = functionName,  logicalResultQ = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  smrObj$M
}

##===========================================================
## M01 Taker
##===========================================================

#' Take M01.
#' @description Takes M01 from the monad object.
#' @param smrObj An SMRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{SMRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
SMRMonTakeM01 <- function( smrObj, functionName = "SMRMonTakeM01" ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "M01", memberPrettyName = "M01", functionName = functionName,  logicalResultQ = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  smrObj$M01
}

##===========================================================
## TagTypeRanges Taker
##===========================================================

#' Take TagTypeRanges.
#' @description Takes TagTypeRanges from the monad object.
#' @param smrObj An SMRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{SMRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
SMRMonTakeTagTypeRanges <- function( smrObj, functionName = "SMRMonTakeTagTypeRanges" ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "TagTypeRanges", memberPrettyName = "TagTypeRanges", functionName = functionName,  logicalResultQ = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  smrObj$TagTypeRanges
}

##===========================================================
## TagTypes Taker
##===========================================================

#' Take TagTypes.
#' @description Takes TagTypes from the monad object.
#' @param smrObj An SMRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{SMRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
SMRMonTakeTagTypes <- function( smrObj, functionName = "SMRMonTakeTagTypes" ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "TagTypes", memberPrettyName = "TagTypes", functionName = functionName,  logicalResultQ = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  smrObj$TagTypes
}

##===========================================================
## ItemColumnName Taker
##===========================================================

#' Take ItemColumnName.
#' @description Takes ItemColumnName from the monad object.
#' @param smrObj An SMRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{SMRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
SMRMonTakeItemColumnName <- function( smrObj, functionName = "SMRMonTakeItemColumnName" ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "ItemColumnName", memberPrettyName = "ItemColumnName", functionName = functionName,  logicalResultQ = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  smrObj$ItemColumnName
}

##===========================================================
## TagToIndexRules Taker
##===========================================================

#' Take TagToIndexRules.
#' @description Takes TagToIndexRules from the monad object.
#' @param smrObj An SMRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{SMRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
SMRMonTakeTagToIndexRules <- function( smrObj, functionName = "SMRMonTakeTagToIndexRules" ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "TagToIndexRules", memberPrettyName = "TagToIndexRules", functionName = functionName,  logicalResultQ = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  smrObj$TagToIndexRules
}

##===========================================================
## ItemToIndexRules Taker
##===========================================================

#' Take ItemToIndexRules.
#' @description Takes ItemToIndexRules from the monad object.
#' @param smrObj An SMRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{SMRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
SMRMonTakeItemToIndexRules <- function( smrObj, functionName = "SMRMonTakeItemToIndexRules" ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "ItemToIndexRules", memberPrettyName = "ItemToIndexRules", functionName = functionName,  logicalResultQ = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  smrObj$ItemToIndexRules
}


##===========================================================
## Data Taker
##===========================================================

#' Take Data.
#' @description Takes Data from the monad object.
#' @param smrObj An SMRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{SMRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
SMRMonTakeData <- function( smrObj, functionName = "SMRMonTakeData" ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "Data", memberPrettyName = "Data", functionName = functionName,  logicalResultQ = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  smrObj$Data
}


##===========================================================
## Get property
##===========================================================

#' Get a member of a SMRMon object
#' @description Places an SMRMon object member to be the pipeline value.
#' @param smrObj An SMRMon object.
#' @param property A string.
#' @return SMR object.
#' @details This function simplifies certain workflows.
#' @family Get functions
#' @export
SMRMonGetProperty <- function( smrObj, property ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !is.character(property) ) {
    warning( "The argument property is expected to be a string.", call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  res <-
    if ( tolower(property) %in% tolower( c( "TagTypes") ) ) {
      smrObj %>% SMRMonTakeTagTypes
    } else if( tolower(property) %in% tolower( c( "TagTypeRanges") ) ) {
      smrObj %>% SMRMonTakeTagTypeRanges
    } else if( tolower(property) %in% tolower( c( "ItemColumnName") ) ) {
      smrObj %>% SMRMonTakeItemColumnName
    } else if( tolower(property) %in% tolower( c( "Matrix", "M") ) ) {
      smrObj %>% SMRMonTakeM
    } else if( tolower(property) %in% tolower( c( "IncidenceMatrix", "M01") ) ) {
      smrObj %>% SMRMonTakeM01
    } else if( tolower(property) %in% tolower( c( "SubMatrices", "SubMatrixes", "ContingencyMatrices", "ContingencyMatrixes") ) ) {
      setNames(
        purrr::map( smrObj %>% SMRMonTakeTagTypes, function(tagType) {
          SMRSubMatrix( smr = smrObj, tagType = tagType )
        }),
        smrObj %>% SMRMonTakeTagTypes
      )
    } else if( tolower(property) %in% tolower( c( "properties") ) ) {
      c( names(smrObj), c( "matrix", "incidenceMatrix", "subMatrices", "properties" ) )
    } else {
      warning("Unknown property specification.", call. = TRUE )
      "Unknown property specification."
    }

  smrObj$Value <- res

  smrObj
}

##===========================================================
## Get sparse matrix property
##===========================================================

#' Get a member of a SMRMon object
#' @description Places an SMRMon object member to be the pipeline value.
#' @param smrObj An SMRMon object.
#' @param property A string.
#' @param tagType Tag type string (sub-matrix name) to get the property of.
#' If NULL the whole recommendation matrix, \code{smrObj$M}, is used.
#' @return SMR object.
#' @details This function simplifies certain workflows.
#' @family Get functions
#' @export
SMRMonGetMatrixProperty <- function( smrObj, property, tagType = NULL ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !is.character(property) ) {
    warning( "The argument property is expected to be a string.", call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  if( is.null(tagType) ) {

    smat <-smrObj %>% SMRMonTakeM

  } else {

    if( !( tagType %in% smrObj$TagTypes ) ) {
      warning( paste0( "Unknown tag type: ", tagType, "." ), call. = TRUE )
      return(SMRMonFailureSymbol)
    }

    smat <- SMRSubMatrix( smr = smrObj, tagType = tagType )
  }

  res <-
    if ( tolower(property) %in% tolower( c( "tags", "columns", "colnames" ) ) ) {
      colnames(smat)
    } else if( tolower(property) %in% tolower( c( "rows", "rownames" ) ) ) {
      rownames(smat)
    } else if( tolower(property) %in% tolower( c( "numberOfColumns") ) ) {
      ncol(smat)
    } else if( tolower(property) %in% tolower( c( "numberOfRows") ) ) {
      nrow(smat)
    } else if( tolower(property) %in% tolower( c( "dim", "dimensions") ) ) {
      dim(smat)
    } else if( tolower(property) %in% tolower( c( "density") ) ) {
      length((smat)@x) / length(smat)
    } else if( tolower(property) %in% tolower( c( "tagTypeWeights", "tagTypeSignificanceFactors") ) ) {
      SMRCurrentTagTypeSignificanceFactors( smrObj )
    } else if( tolower(property) %in% tolower( c( "properties") ) ) {
      sort( c( "tags", "columns", "rows",
               "numberOfColumns", "numberOfRows",
               "dim", "dimensions", "density",
               "tagTypeWeights", "tagTypeSignificanceFactors",
               "properties") )
    } else {
      warning("Unknown property specification.", call. = TRUE )
      "Unknown property specification."
    }

  smrObj$Value <- res

  smrObj
}

##===========================================================
## Create from data
##===========================================================

#' Make an SMRMon object from a transactions data frame
#' @description Creates a sparse matrix recommender from transactions data and a list of tag types,
#' and makes is an SMRMon object.
#' @param smrObj An SMRMon object.
#' @param data The transactions data frame.
#' @param tagTypes The name of the column containing the categorical tags.
#' @param itemColumnName The name of the column containing the unique items.
#' @param ... Additional parameters for \code{\link{SMRMonCreateFromMatrices}}.
#' @details An S3 object is returned that is list with class attribute set to "SMR".
#' @return SMR object.
#' @family Creation functions
#' @export
SMRMonCreate <- function( smrObj, data = SMRMonTakeData(smrObj), tagTypes = names(data)[-1], itemColumnName = names(data)[1], ... ) {

  ## We allow anything to be smrObj .
  ## if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  res <- SMRCreate( dataRows = data, tagTypes = tagTypes, itemColumnName = itemColumnName, ... )

  res$Value <- NULL
  res$Data <- data
  class(res) <- "SMR"

  res
}


##===========================================================
## Create from matrices
##===========================================================

#' Creation an SMRMon object with a list of matrices.
#' @description Creates a sparse matrix recommender from a list of matrices and a corresponding list of tag types,
#' and makes is an SMRMon object.
#' @param smrObj An SMRMon object.
#' @param matrices A list of matrices to be spliced into a metadata matrix.
#' @param tagTypes Vector of matrix names.
#' @param itemColumnName The column name of recommender items (in data and recommendations).
#' @param imposeSameRowNamesQ Should the union of the row names be imposed on each matrix?
#' @param addTagTypesToColumnNamesQ Should the tag types be added as prefixes
#' to the column names of the corresponding sub-matrices?
#' @param sep Separator for the prefixes of the columns names.
#' @details An S3 object is returned that is list with class attribute set to "SMR".
#' @return An SMRMon object.
#' @family Creation functions
#' @export
SMRMonCreateFromMatrices <- function( smrObj, matrices, tagTypes = names(matrices), itemColumnName = "Item", imposeSameRowNamesQ = TRUE, addTagTypesToColumnNamesQ = FALSE, sep = ":"  ) {

  ## We allow anything to be smrObj .
  ## if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  res <- SMRCreateFromMatrices( matrices = matrices,
                                tagTypes = tagTypes,
                                itemColumnName = itemColumnName,
                                imposeSameRowNamesQ = imposeSameRowNamesQ,
                                addTagTypesToColumnNamesQ = addTagTypesToColumnNamesQ,
                                sep = sep )

  if( is.null(res) ) { return(SMRMonFailureSymbol) }

  res$Value <- NULL
  class(res) <- "SMR"

  if( SMRMonMemberPresenceCheck( smrObj = smrObj, memberName = "Data", logicalResultQ = TRUE, warningQ = FALSE ) ) {
    res$Data <- smrObj %>% SMRMonTakeData
  }

  res
}


##===========================================================
## Apply term-weight functions
##===========================================================

#' Apply term-weight functions.
#' @description Re-weight the entries of the recommender matrix (per tag type.)
#' @param smrObj A SMRMon object.
#' @param globalWeightFunction Global weight function.
#' @param localWeightFunction Local weight function.
#' @param normalizerFunction Normalizer function.
#' @return A SMRMon object.
#' @details This function calls
#' \code{\link{SparseMatrixRecommender::SMRApplyTermWeightFunctions}} for each
#' sub-matrix of \code{smrObj$M}.
#' @export
SMRMonApplyTermWeightFunctions <- function( smrObj, globalWeightFunction = "IDF", localWeightFunction = "None", normalizerFunction = "Cosine" ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "M", memberPrettyName = "M", functionName = "SMRMonApplyTermWeightFunctions",  logicalResultQ = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  smats <-
    purrr::map( setNames( smrObj %>% SMRMonTakeTagTypes, smrObj %>% SMRMonTakeTagTypes), function(tt) {

      SparseMatrixRecommender::SMRApplyTermWeightFunctions( docTermMat = SMRSubMatrix( smr = smrObj, tagType = tt ),
                                                            globalWeightFunction = globalWeightFunction,
                                                            localWeightFunction = localWeightFunction,
                                                            normalizerFunction = normalizerFunction )
    })

  smrObj$M <- do.call( cbind, smats )
  smrObj$M01 <- smrObj$M

  smrObj
}


##===========================================================
## Long form
##===========================================================

#' Long form data frame representation.
#' @description Creates a long form data frame for the recommendation matrix of an SMRMon object.
#' @param smrObj An SMRMon object.
#' @param items A character vector of items.
#' If NULL all elements of \code{rownames(smrObj$M)} are used.
#' @param tagTypesQ Should the tag types be included or not?
#' @param summarizeTagsQ Should the tags for each unique
#' item-and-tag-type pair be summarized or not?
#' @param nTopTags Number of top tags.
#' Used when \code{summarizeTagsQ = TRUE}.
#' @details The result data frame is assigned to \code{smrObj$Value}.
#' @return An SMRMon object.
#' @family Data functions
#' @export
SMRMonGetLongFormData <- function( smrObj, items = NULL, tagTypesQ = TRUE, summarizeTagsQ = FALSE, nTopTags = 5 ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !( is.null(items) || is.character(items) && mean( items %in% rownames(smrObj$M) ) == 1 ) ) {
    warning( "The argument items is expected a character vector with elements from rownames(smrObj$M).", call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  if( is.null(items) ) {
    smat <- smrObj %>% SMRMonTakeM
  } else {
    smat <- (smrObj %>% SMRMonTakeM)[items, , drop = F]
  }

  if( tagTypesQ ) {

    ## Make a long form for each sub-matrices and cbind the corresponding tag types.


    smatColNames <-  c( smrObj %>% SMRMonTakeItemColumnName, "Tag", "Value" )

    res <-
      purrr::map_df( smrObj %>% SMRMonTakeTagTypes, function(tt) {

        m <- SMRSubMatrixOfMatrix( M = smat, ranges = smrObj %>% SMRMonTakeTagTypeRanges, tagType = tt )

        if( is.null(m) || nrow(m) == 0 || sum(m@x) == 0) {
          NULL
        } else {
          m <- setNames( SMRSparseMatrixToTriplets( smat = m ), smatColNames )
          cbind( m, TagType = tt, stringsAsFactors = FALSE )
        }

      })

    smatColNames <- c( smrObj %>% SMRMonTakeItemColumnName, "TagType", "Tag", "Value" )

    res <-
      res %>%
      dplyr::select_at( .vars = smatColNames )

  } else {

    ## Probably faster?
    smatColNames <- c( smrObj %>% SMRMonTakeItemColumnName, "Tag", "Value" )

    res <- setNames( SMRSparseMatrixToTriplets( smat = smat ), smatColNames )

  }

  res <- res %>% dplyr::arrange_at( .vars = smatColNames )

  ## Summarize tags.
  if ( summarizeTagsQ ) {

    if( ! is.numeric(nTopTags) ) {
      nTopTags <- NA
    }

    if( tagTypesQ ) {

      res <-
        res %>%
        dplyr::group_by_at( .vars = c( smrObj %>% SMRMonTakeItemColumnName, "TagType" ) ) %>%
        dplyr::arrange( desc(Value) ) %>%
        dplyr::summarize( NumberOfTags = length(Tag), TopTags = paste( Tag[ 1 : min(nTopTags, length(Tag), na.rm = T) ], collapse = ";" ) ) %>%
        dplyr::ungroup()

    } else {

      res <-
        res %>%
        dplyr::group_by_at( .vars = c( smrObj %>% SMRMonTakeItemColumnName ) ) %>%
        dplyr::arrange( desc(Value) ) %>%
        dplyr::summarize( NumberOfTags = length(Tag), TopTags = paste( Tag[ 1 : min(nTopTags, length(Tag), na.rm = T)], collapse = ";" ) ) %>%
        dplyr::ungroup()

    }
  }

  ## Returned result
  smrObj$Value <- res

  smrObj
}


##===========================================================
## Summarize item
##===========================================================

#' Summarize an item
#' @description Summarizes the recommender matrix data
#' for a given item.
#' (A row of the recommender matrix \code{smrObj$M}.)
#' @param smrObj An SMRMon object.
#' @param item A string.
#' (That is one of \code{rownames(smrObj$M)}.)
#' @param tagTypesQ Should the tag types be included or not?
#' @param nTopTags Number of top tags for the \code{"TagsSummary"}
#' element of the result.
#' @details
#' The result is a list with named elements assigned to \code{smrObj$Value}.
#' @return A SMRMon object
#' @export
SMRMonSummarizeItem <- function( smrObj, item, tagTypesQ = TRUE, nTopTags = 5 ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( is.null(item) ) {
    item <- smrObj %>% SMRMonTakeValue
  }

  if( !( is.character(item) && length(item) == 1 && (item %in% rownames(smrObj$M) ) ) ) {
    warning( "The value of the argument item is expected to be one of rownames(smrObj$M).", call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  ## Tags profile.
  dfProfile <-
    smrObj %>%
    SMRMonGetLongFormData( items = c(item), tagTypesQ = tagTypesQ ) %>%
    SMRMonTakeValue

  if( tagTypesQ ) {

    dfProfile <- dfProfile %>% dplyr::arrange( TagType, desc(Value) )

  } else {

    dfProfile <- dfProfile %>% dplyr::arrange( desc(Value) )

  }

  ## Tags summary: number of tags, top tags/outliers.
  dfTagsSummary <-
    smrObj %>%
    SMRMonGetLongFormData( items = c(item), tagTypesQ = tagTypesQ, summarizeTagsQ = TRUE, nTopTags = nTopTags ) %>%
    SMRMonTakeValue

  ## Top tags / tag outliers.
  ## We assume something like TF-IDF or contingency matrix entries.
  ## TBD

  ## Number of NNs in the expected vicinity.
  ## TBD...

  ## Result
  smrObj$Value <- list( Profile = dfProfile, TagsSummary = dfTagsSummary )
  smrObj
}


##===========================================================
## Get history data frame
##===========================================================

#' From a history specification into a data frame
#' @description Transforms a history specification into a data frame.
#' @param smrObj An SMRMon object.
#' @param history History specification.
#' A data frame with columns \code{c("Rating", "Item")};
#' a numeric vector named elements, the names being items;
#' a character vector, the correspond ratings assumed all to be 1.
#' @param functionName A string that is a name of this function or a delegating function.
#' @param warningQ Should a warning be issued if \code{history} is of unknown type?
#' @details The result data frame is with columns "Score", \code{smr$ItemColumnName};
#' assigned to \code{smrObj$Value}.
#' If \code{history = NULL} then \code{smrObj$Value} is used.
#' @return A SMRMon object
#' @export
SMRMonGetHistoryDataFrame <- function( smrObj, history, functionName = "SMRMonGetHistoryDataFrame", warningQ = TRUE ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( is.null(history) ) {
    history <- smrObj %>% SMRMonTakeValue
  }

  if( is.data.frame(history) && sum( c("Rating", "Item", smrObj$ItemColumnName ) %in% names(history) ) == 2 ) {

    names(history) <- gsub( smrObj$ItemColumnName, "Item", names(history) )

    historyRatings <- history$Rating
    historyItems <- history$Item

  } else if( is.numeric(history) && !is.null(names(history)) ) {

    historyRatings <- setNames( history, NULL)
    historyItems <- names(history)

  } else if( is.character(history) ) {

    historyRatings <- rep_len( 1, length.out = length(history) )
    historyItems <- history

  } else {

    if( warningQ ) {
      warning( paste0( "Unknown history type from the function ", functionName, "." ), call. = TRUE )
    }
    return(SMRMonFailureSymbol)

  }

  smrObj$Value <- data.frame( Rating = historyRatings, Item = historyItems, stringsAsFactors = FALSE )

  smrObj
}


##===========================================================
## Get profile data frame
##===========================================================

#' From a profile specification into a data frame
#' @description Transforms a profile specification into a data frame.
#' @param smrObj An SMRMon object.
#' @param profile Profile specification.
#' A data frame with columns \code{c("Score", "Tag")};
#' a numeric vector named elements, the names being items;
#' a character vector, the correspond ratings assumed all to be 1.
#' @param functionName A string that is a name of this function or a delegating function.
#' @param ignoreUnknownTagsQ Should the unknown tags be ignored or not?
#' @param warningQ Should a warning be issued if \code{profile} is of unknown type?
#' @details The result data frame is with columns "Score", \code{smr$ItemColumnName};
#' assigned to \code{smrObj$Value}.
#' If \code{profile = NULL} then \code{smrObj$Value} is used.
#' @return A SMRMon object
#' @export
SMRMonGetProfileDataFrame <- function( smrObj, profile, functionName = "SMRMonGetProfileDataFrame", ignoreUnknownTagsQ = FALSE, warningQ = TRUE ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( is.null(profile) ) {
    profile <- smrObj %>% SMRMonTakeValue
  }

  if( is.data.frame(profile) && sum( c("Score", "Tag" ) %in% names(profile) ) == 2 ) {

    profileScores <- profile$Score
    profileTags <- profile$Tag

  } else if( is.numeric(profile) && !is.null(names(profile)) ) {

    profileScores <- setNames( profile, NULL)
    profileTags <- names(profile)

  } else if( is.character(profile) ) {

    profileScores <- rep_len( 1, length.out = length(profile) )
    profileTags <- profile

  } else {

    if(warningQ) {
      warning( paste0( "Unknown profile type from the function ", functionName, "." ), call. = TRUE )
    }
    return(SMRMonFailureSymbol)

  }

  res <- data.frame( Score = profileScores, Tag = profileTags, stringsAsFactors = FALSE )

  if( ignoreUnknownTagsQ ) {
    res <- res[ res$Tag %in% colnames(smrObj$M), ]
  }

  smrObj$Value <- res

  smrObj
}


##===========================================================
## Get profile data frame
##===========================================================

#' From a profile specification into a vector
#' @description Transforms a profile specification into a sparse matrix
#' with one row.
#' @param smrObj An SMRMon object.
#' @param profile Profile specification.
#' A data frame with columns \code{c("Score", "Tag")};
#' a numeric vector named elements, the names being tags;
#' a character vector, the correspond ratings assumed all to be 1.
#' @param tagType Tag type over which the vector is made.
#' @param uniqueColumnsQ Should the tags in the profile have unique indexes in the columns of \code{smrObj$M}?
#' @param functionName A string that is a name of this function or a delegating function.
#' @param ignoreUnknownTagsQ Should the unknown tags be ignored or not?
#' @param warningQ Should a warning be issued if \code{profile} is of unknown type?
#' @details The result profile vector (sparse matrix) is
#' assigned to \code{smrObj$Value}.
#' If \code{profile = NULL} then \code{smrObj$Value} is used.
#' @return A SMRMon object
#' @export
SMRMonGetProfileVector <- function( smrObj, profile, tagType = NULL, uniqueColumnsQ = TRUE, functionName = "SMRMonGetProfileVector", ignoreUnknownTagsQ = FALSE, warningQ = TRUE ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( is.null(profile) ) {
    profile <- smrObj %>% SMRMonTakeValue
  }

  if( SMRSparseMatrixQ( profile ) ) {

    if( !( "dgCMatrix" %in% class(profile)) ) {
      profile <- as( profile, "dgCMatrix" )
    }

    if( ncol(profile) != 1 ) {
      warning( paste0( "The profile vector given to the function ", functionName,
                       " is expected to be a sparse matrix with one column."),
               call. = TRUE )
      return(SMRMonFailureSymbol)
    }

    if( ignoreUnknownTagsQ ) {
      profile <- profile[ rownames(profile) %in% colnames(smrObj$M), , drop = FALSE]
    }

    if( nrow(profile) != ncol(smrObj$M) ) {
      warning( paste0( "The profile vector given to the function ", functionName,
                       " is expected to be a sparse matrix with ", ncol(smrObj$M),
                       " rows."),
               call. = TRUE )
      return(SMRMonFailureSymbol)
    }

  } else if ( is.data.frame(profile) ) {

    if( mean( c('Score', 'Tag') %in% colnames(profile) ) < 1 ) {
      warning( paste0( "The profile data frame given to the function ", functionName,
                       " is expected to have the columns c('Score', 'Tag')."),
               call. = TRUE )
      return(SMRMonFailureSymbol)
    }

    if( ignoreUnknownTagsQ ) {
      profile <- profile[ profile$Tag %in% colnames(smrObj$M), ]
    }

    profile <- SMRProfileDFToVector( smr = smrObj,
                                     profileDF = profile,
                                     tagType = tagType,
                                     uniqueColumns = uniqueColumnsQ )

  } else if ( is.character(profile) ) {

    if( ignoreUnknownTagsQ ) {
      profile <- profile[ names(profile) %in% colnames(smrObj$M) ]
    }

    profile <- SMRProfileDFToVector( smr = smrObj,
                                     profileDF = data.frame( Score = 1, Tag = profile, stringsAsFactors = FALSE),
                                     tagType = tagType,
                                     uniqueColumns = uniqueColumnsQ )

  } else {

    if(warningQ) {
      warning( paste0( "Unknown profile vector type from the function ", functionName, "." ), call. = TRUE )
    }
    return(SMRMonFailureSymbol)

  }

  smrObj$Value <- profile

  smrObj
}


##===========================================================
## Predicates
##===========================================================

#' Is the argument a profile specification.
#' @description Tests is the argument a profile.
#' If \code{smrObj$M} is a (recommendation) sparse matrix then the profile tags
#' are tested are they known columns of \code{smrObj$M}.
#' @param smrObj An SMRMon object.
#' @param spec Data to be tested. If NULL \code{smrObj$Value} is tested.
#' @param functionName A string that is a name of this function or a delegating function.
#' @param logicalResultQ Should the result be a logical value?
#' @return SMR object.
#' @details This function simplifies certain workflows.
#' @family Predicate functions
#' @export
SMRMonProfileSpecificationQ <- function( smrObj, spec, functionName = "SMRMonProfileSpecificationQ", logicalResultQ = FALSE ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  profile <- SMRMonUnit() %>% SMRMonGetProfileDataFrame( spec, functionName = functionName )

  if( SMRMonFailureQ(profile) ) {

    res <- FALSE

  } else {

    if( SMRMonMemberPresenceCheck( smrObj = smrObj, memberName = "M", functionName = functionName, logicalResult = TRUE) ) {

      res <- mean(profile$Tag %in% colnames(smrObj$M))

      if( 0 < res && res < 1 ) {
        warning( paste0( "Some profile tags are unknown when invoking ", functionName, "."), call. = TRUE )
        res <- TRUE
      } else if ( res == 0 ) {
        warning( paste0( "All profile tags are unknown when invoking ", functionName, "."), call. = TRUE )
        res <- FALSE
      } else {
        res <- TRUE
      }

    } else {
      res <- TRUE
    }

  }

  if( logicalResultQ ) { return(res) }

  smrObj$Value <- profile
  return(smrObj)
}


##===========================================================
## Get top recommendations
##===========================================================

#' Get top recommendations
#' @description Computes the top recommendations for a history or profile argument
#' or monad's value.
#' @param smrObj An SMRMon object.
#' @param spec A history or profile specification.
#' If NULL \code{smrObj$Value} is (tried to be) used.
#' @return SMR object.
#' @details This function simplifies certain workflows.
#' (Very inefficient implementation at this point.)
#' @family Recommendations computation functions
#' @export
SMRMonGetTopRecommendations <- function( smrObj, spec = NULL, nrecs = 12, ... ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  lsArgs <- list(...)

  if ( is.null(spec) ) {

    if( !("ignoreUnknownTagsQ" %in% names(lsArgs)) ) {

      res <- smrObj %>% SMRMonRecommend( smrObj %>% SMRMonTakeValue, nrecs = nrecs, warningQ = FALSE, ... )

      if( !SMRMonFailureQ(res) ) { return(res) }

    }

    res <- smrObj %>% SMRMonRecommendByProfile(  smrObj %>% SMRMonTakeValue, nrecs = nrecs, warningQ = FALSE, ... )

    if( SMRMonFailureQ(res) ) {
      warning( "The monad object 'Value' is not a history or profile specification.", call. = TRUE )
    }

  } else {

    if( !("ignoreUnknownTagsQ" %in% names(lsArgs)) ) {

      res <- smrObj %>% SMRMonRecommend( history = spec, nrecs = nrecs, warningQ = FALSE, ... )

      if( !SMRMonFailureQ(res) ) { return(res) }

    }

    res <- smrObj %>% SMRMonRecommendByProfile( profile = spec, nrecs = nrecs, warningQ = FALSE, ... )

    if( SMRMonFailureQ(res) ) {
      warning( "The argument spec is not a history or profile specification.", call. = TRUE )
    }
  }

  res
}


##===========================================================
## Recommend by history
##===========================================================

#' Compute recommendations
#' @description Recommend items based on history of consumption.
#' @param smrObj An SMRMon object.
#' @param history History specification.
#' A data frame with columns \code{c("Rating", "Item")};
#' a numeric vector named elements, the names being items;
#' a character vector, the correspond ratings assumed all to be 1.
#' @param nrecs Number of recommendations to be returned.
#' @param removeHistoryQ Should the history be removed from the recommendations?
#' @param normalizeQ Should the scores be normalized by the maximum score or not?
#' @param warningQ Should a warning be issued if \code{history} is of unknown type?
#' @details The recommendations result is a
#' data frame with columns "Score", "Index", \code{smrObj$ItemColumnName};
#' assigned to \code{smrObj$Value}.
#' @return A SMRMon object
#' @family Recommendations computation functions
#' @export
SMRMonRecommend <- function( smrObj, history, nrecs = 12, removeHistoryQ = FALSE, normalizeQ = FALSE, warningQ = TRUE ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "M", memberPrettyName = "M", functionName = "SMRMonRecommend",  logicalResultQ = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  smrObj <- smrObj %>% SMRMonGetHistoryDataFrame( history = history, functionName = "SMRMonRecommend", warningQ = warningQ )

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  history <- smrObj %>% SMRMonTakeValue

  res <- SMRRecommendations( smr = smrObj, userHistoryItems = history$Item, userRatings = history$Rating, nrecs = nrecs, removeHistory = removeHistoryQ )

  if( normalizeQ ) {
    maxScore <- max(res$Score)
    if( maxScore != 0 ) { res$Score <- res$Score / maxScore }
  }

  smrObj$Value <- res

  smrObj
}


##===========================================================
## Recommend by profile
##===========================================================

#' Recommendations using a profile
#' @description Recommend items based on a sparse matrix and a specified profile indices and scores.
#' @param smrObj An SMRMon object.
#' @param profile Profile specification.
#' A data frame with columns \code{c("Score", "Tag")};
#' a numeric vector with named elements, the names being items;
#' a character vector, the correspond ratings assumed all to be 1;
#' a sparse matrix with 1 column and a number of rows that equals \code{ncol(smrObj$M)}.
#' @param nrecs Number of recommendations to be returned.
#' @param normalizeQ Should the scores be normalized by the maximum score or not?
#' @param ignoreUnknownTagsQ Should the unknown tags be ignored or not?
#' @param warningQ Should a warning be issued if \code{profile} is of unknown type?
#' @details The recommendations result is a
#' data frame with columns "Score", "Index", \code{smr$ItemColumnName};
#' assigned to \code{smrObj$Value}.
#' @return A SMRMon object
#' @family Recommendations computation functions
#' @export
SMRMonRecommendByProfile <- function( smrObj, profile, nrecs = 12, normalizeQ = FALSE, ignoreUnknownTagsQ = FALSE, warningQ = TRUE ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "M", memberPrettyName = "M", functionName = "SMRMonRecommendByProfile",  logicalResultQ = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  if( SparseMatrixRecommender::SMRSparseMatrixQ(profile) ) {

    smrObj <- smrObj %>% SMRMonGetProfileVector( profile = profile, functionName = "SMRMonRecommendByProfile", ignoreUnknownTagsQ = ignoreUnknownTagsQ, warningQ = warningQ )

    if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

    profile <- smrObj %>% SMRMonTakeValue

    res <- SMRRecommendationsByProfileVector( smr = smrObj, profileVec = profile, nrecs = nrecs )

  } else {

    smrObj <- smrObj %>% SMRMonGetProfileDataFrame( profile = profile, functionName = "SMRMonRecommendByProfile", ignoreUnknownTagsQ = ignoreUnknownTagsQ, warningQ = warningQ )

    if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

    profile <- smrObj %>% SMRMonTakeValue

    res <- SMRRecommendationsByProfileDF( smr = smrObj,
                                          profile = data.frame( Score = profile$Score, Tag = profile$Tag, stringsAsFactors = FALSE ),
                                          nrecs = nrecs )

  }

  if( normalizeQ ) {
    maxScore <- max(res$Score)
    if( maxScore != 0 ) { res$Score <- res$Score / maxScore }
  }

  smrObj$Value <- res

  smrObj
}

##===========================================================
## Batch recommend by matrix
##===========================================================

#' Batch recommend
#' @description For each row of a specified matrix of profiles compute recommendations.
#' @param smrObj An SMRMon object.
#' @param data A matrix of profiles that has the same number of columns as
#' \code{smrObj$M}, or a vector of items (i.e. rownames of \code{smrObj$M}.)
#' If NULL then \code{smrObj$M} is used.
#' @param nrecs Number of recommendations to be returned.
#' If NULL all non-zero score recommendations are returned.
#' @param removeHistoryQ Should the history be removed from the recommendations?
#' @param normalizeQ Should the scores be normalized by the maximum score or not?
#' @param targetColumnName The column name for the \code{data} rownames in the result data frame.
#' (I.e. the column in the recommendations result data frame that has \code{rownames(mat)} as values.)
#' @details The recommendations result is a
#' data frame with columns \code{c(targetColumnName "Score", "Index", smrObj$ItemColumnName)};
#' assigned to \code{smrObj$Value}. The following steps are taken:
#' (1) Make sure the data argument is \code{"dgCMatrix"}.
#' (2) Compute matrix-matrix product for the recommendations.
#' (3) If specified, remove history.
#' (4) Keep the specified top number of recommendations only.
#' (5) If specified, normalize per data element the scores by the maximum score.
#' (6) Return results.
#' @return A SMRMon object
#' @family Recommendations computation functions
#' @export
SMRMonBatchRecommend <- function( smrObj, data = NULL, nrecs = 12, removeHistoryQ = FALSE, normalizeQ = FALSE, targetColumnName = "ProfileID" ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "M", memberPrettyName = "M", functionName = "SMRMonBatchRecommend",  logicalResultQ = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  ## Processing nrecs
  if( ! ( is.null( nrecs) || is.numeric(nrecs) && length(nrecs) == 1 && nrecs > 0 ) ) {
    warning("The argument nrecs is expected to be NULL or a positive integer.", call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  if( is.null(nrecs) ) { nrecs <- nrow(smrObj$M) }

  ## Processing mat
  if( ! ( is.null(data) ||
          is.character(data) && mean( data %in% rownames(smrObj$M) ) == 1 ||
          SMRSparseMatrixQ(data) && ncol(data) == ncol(smrObj$M) ) ) {
    warning(
      paste(
        "The argument data is expected to be NULL, a vector of items (rownames of smrObj$M),",
        "a matrix or sparse matrix with the same number of columns as smrObj$M,",
        ncol(smrObj$M), "."), call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  if( is.null(data) ) { data <- smrObj$M }

  if( is.character(data) ) { data <- smrObj$M0[data, ] }


  data <- as( data, "dgCMatrix")

  if( is.null(row.names(data)) ) { rownames(data) <- as.character(1:nrow(data)) }

  ## Recommendations
  matRecs <- smrObj$M %*% t(data)
  dfRecsMat <- SMRSparseMatrixToTriplets( smat = matRecs )

  ## Resolving name collusion
  if( targetColumnName == (smrObj %>% SMRMonTakeItemColumnName) ) {
    targetColumnName <- paste0(targetColumnName, ".2")
    warning( paste( "The argument targetColumnName has the same values as smrObj$ItemColumnName. Using", targetColumnName, "instead." ), call. = TRUE )
  }

  colnames(dfRecsMat) <- c( smrObj %>% SMRMonTakeItemColumnName, targetColumnName, "Score" )
  dfRecsMat <- dfRecsMat %>% dplyr::filter( Score > 0 )

  ## Remove history if using smrObj$M
  if( removeHistoryQ && !SMRSparseMatrixQ(data) ) {
    dfRecsMat <- dfRecsMat[ dfRecsMat[[1]] != dfRecsMat[[2]], ]
  }

  ## Reverse sort and get top recommendations
  dfRecsMat <-
    dfRecsMat %>%
    dplyr::group_by_at( targetColumnName ) %>%
    dplyr::arrange( dplyr::desc(Score) ) %>%
    dplyr::filter( dplyr::row_number() <= nrecs ) %>%
    dplyr::ungroup() %>%
    dplyr::select_at( c(targetColumnName, "Score", smrObj %>% SMRMonTakeItemColumnName) )

  dfRecsMat <- setNames(dfRecsMat, c(targetColumnName, "Score", smrObj %>% SMRMonTakeItemColumnName) )

  ## Normalize
  if( normalizeQ ) {
    dfRecsMat <-
      dfRecsMat %>%
      dplyr::group_by_at( targetColumnName ) %>%
      dplyr::mutate( MaxScore = max(abs(Score)) ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate( Score = ifelse( MaxScore > 0, Score / MaxScore, Score ) ) %>%
      dplyr::mutate( MaxScore = NULL )
  }

  ## Order scores descendingly
  dfRecsMat <- dfRecsMat[ order( dfRecsMat[[targetColumnName]], - dfRecsMat[["Score"]] ), ]

  smrObj$Value <- dfRecsMat

  smrObj
}


##===========================================================
## Find profile
##===========================================================

#' Profile for a history specification
#' @description Find the profile that corresponds to a history specification.
#' @param smrObj An SMRMon object.
#' @param history History specification.
#' A data frame with columns \code{c("Rating", "Item")};
#' a numeric vector named elements, the names being items;
#' a character vector, the correspond ratings assumed all to be 1.
#' @param tagTypesQ Should the tag types be included or not?
#' @details
#' The profile data frame is assigned to \code{smrObj$Value}.
#' If \code{tagTypesQ = FALSE} then its are columns "Score", "Index", "Tag".
#' If \code{tagTypesQ = TRUE} then its are columns "Score", "Index", "TagType", "Tag".
#' @return A SMRMon object
#' @export
SMRMonProfile <- function( smrObj, history, tagTypesQ = FALSE ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "M", memberPrettyName = "M", functionName = "SMRMonProfile",  logicalResultQ = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  smrObj <- smrObj %>% SMRMonGetHistoryDataFrame( history = history, functionName = "SMRMonProfile" )

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  history <- smrObj %>% SMRMonTakeValue

  res <- SMRProfileDF( smr = smrObj, itemHistory = history )

  if( tagTypesQ ) {

    ttInds <- c( smrObj$TagTypeRanges$Begin, smrObj$TagTypeRanges$End[ nrow(smrObj$TagTypeRanges) ])

    tts <- findInterval( res$Index, ttInds, all.inside = TRUE )

    res <- cbind( res, TagType = rownames(smrObj$TagTypeRanges)[ tts ], stringsAsFactors = FALSE )

    res <- res[ , c( "Score", "Index", "TagType", "Tag") ]
  }

  smrObj$Value <- res

  smrObj
}


##===========================================================
## Join accross
##===========================================================

#' Extend recommendations with extra data
#' @description Joins a recommendations data with a given data frame.
#' @param smrObj An SMRMon object.
#' @param data A data frame.
#' @details The argument \code{data} is expected to have a column with name
#' \code{smrObj$ItemColumnName} or a inner join column that is specified
#' with the argument \code{by}.
#' The name of the function corresponds a Mathematica function (\code{JoinAcross}).
#' @return An SMRMon object.
#' @family Recommendations computation functions
#' @export
SMRMonJoinAcross <- function( smrObj, data, by = smrObj$ItemColumnName ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  recs <- smrObj %>% SMRMonTakeValue

  if( !is.data.frame(recs) ) {
    warning( "The element smrObj$Value is expected to be a (recommendations) data frame.", call. = TRUE )
    return( SMRMonFailureSymbol )
  }

  if( is.null(names(by)) ) {
    by <- setNames( c(by), smrObj$ItemColumnName )
  }

  recs <- dplyr::inner_join( recs, data, by = by )
  smrObj$Value <- recs

  smrObj
}


##===========================================================
## Classify by profile
##===========================================================

#' Classification with a profile
#' @description Classify a profile data frame into the column names of a tag type sub-matrix.
#' @param smrObj A sparse matrix recommender.
#' @param tagType Tag type for which the classification is done.
#' @param profile (Scored) profile tags.
#' A data frame with columns \code{c("Score", "Tag")};
#' a numeric vector named elements, the names being items;
#' a character vector, the correspond ratings assumed all to be 1.
#' @param nTopNNs Number of top nearest neighbors to be used in the derive the classification.
#' If NULL one hundred top nearest neighbors are used.
#' (More precisely \code{min( 100, nrow(smrObj$M) )}.)
#' @param voting Should simple voting be used or a weighted sum?
#' @param maxNumberOfLabels The maximum number of labels to be returned;
#' if NULL all found labels are returned.
#' @param normalizeQ Should the scores be normalized?
#' (By dividing by the maximum score.)
#' @details The classification result is a list of scored tags that is assigned
#' to \code{smrObj$Value}.
#' This function is based in \code{\link{SMRClassifyByProfileVector}}.
#' @return An SMRMon object.
#' @export
SMRMonClassifyByProfile <- function( smrObj, tagType, profile, nTopNNs = NULL,
                                     voting = FALSE, dropZeroScoredLabels = TRUE, maxNumberOfLabels = NULL, normalizeQ = TRUE ) {

  ## The following code shares a lot the beginning of SMRMonRecommendByProfile.
  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "M", memberPrettyName = "M", functionName = "SMRMonClassifyByProfile",  logicalResultQ = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  if( is.null(nTopNNs) ) {
    nTopNNs <- min( 100, nrow(smrObj$M) )
  }

  if( SparseMatrixRecommender::SMRSparseMatrixQ(profile)  ) {

    profileVec <- profile

  } else {

    if( is.data.frame(profile) && sum( c("Score", "Tag" ) %in% names(profile) ) == 2 ) {

      profileScores <- profile$Score
      profileTags <- profile$Tag

    } else if( is.numeric(profile) && !is.null(names(profile)) ) {

      profileScores <- setNames( profile, NULL)
      profileTags <- names(profile)

    } else if( is.character(profile) ) {

      profileScores <- rep_len( 1, length.out = length(profile) )
      profileTags <- profile

    } else {

      warning( "Unknown profile type.", call. = TRUE )
      return(SMRMonFailureSymbol)

    }

    profile <- data.frame( Score = profileScores, Tag = profileTags, stringsAsFactors = FALSE )

    profileVec <- SMRProfileDFToVector( smr = smrObj, profileDF = profile )
  }

  clRes <- SMRClassifyByProfileVector( smr = smrObj, tagType = tagType, profileVec = profileVec,
                                       nTopNNs = nTopNNs,
                                       voting = voting, dropZeroScoredLabels = dropZeroScoredLabels, maxNumberOfLabels = maxNumberOfLabels, normalizeQ = normalizeQ )

  smrObj$Value <- clRes

  smrObj
}


##===========================================================
## Prove
##===========================================================

#' Prove a recommendation to a profile or history
#' @description
#' If \code{proofType = "metadata"} then for a given profile and a recommended item
#' finds scored metadata tags that appear in both the given profile and the profile
#' of the recommended item.
#' If \code{proofType = "history"} then for a given history and a recommended item
#' finds the items of the history that are the closest to recommended item.
#' @param smrObj An SMRMon object.
#' @param spec A specification that is a profile or a history.
#' @param items One or more item-indexes or item-names to make proofs for.
#' @param type One of "metadata" or "history".
#' Must be a row index or row name of \code{smrObj$M}.
#' @param normalizeScoresQ Should the proof scores be normalized or not?
#' @param style Proof style derivation; one of "intersection", "multiplication".
#' @param outlierIdentifierParameters Outlier identifier parameters or a parameters finding function.
#' If NULL all tags are returned.
#' @param warningQ Should warnings be given?
#' @details The result is assigned to \code{smrObj$Value}.
#' For some data and outlier identifier parameters the proofs data frame might turn out empty.
#' In those cases warning a given if \code{warningQ = TRUE}.
#' The argument \code{style} has an effect only when \code{type = "metadata"}.
#' @return An SMRMon object.
#' @family Recommendations computation functions
#' @export
SMRMonProve <- function( smrObj, spec, items, type = "metadata", normalizeScoresQ = TRUE, style = "intersection", outlierIdentifierParameters = NULL, warningQ = TRUE ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( tolower(type) == "metadata") {

    dfProfile <-
      smrObj %>%
      SMRMonGetProfileDataFrame( profile = spec, functionName = "SMRMonProveByMetadata", warningQ = TRUE ) %>%
      SMRMonTakeValue

    if( SMRMonFailureQ(dfProfile) ) { return(SMRMonFailureSymbol) }

    res <-
      purrr::map_df( items, function(x) {
        cbind( Recommended = x,
               SMRMetadataProofs( smr = smrObj, toBeLovedItem = x, profile = dfProfile, normalizeScores = normalizeScoresQ, style = style ),
               stringsAsFactors = FALSE )
      })


  } else if ( tolower(type) == "history" ) {

    dfHistory <-
      smrObj %>%
      SMRMonGetHistoryDataFrame( history = spec, functionName = "SMRMonProveByHistory", warningQ = TRUE ) %>%
      SMRMonTakeValue

    if( SMRMonFailureQ(dfHistory) ) { return(SMRMonFailureSymbol) }

    res <-
      purrr::map_df( items, function(x) {
        cbind( Recommended = x,
               SMRHistoryProofs( smr = smrObj, toBeLovedItem = x, history = dfHistory, normalizeScores = normalizeScoresQ ),
               stringsAsFactors = FALSE )
      })

    names(res) <- c( names(res)[1:(ncol(res)-1)], smrObj$ItemColumnName )

  } else {
    warning( "The argument proofType is expected to be one 'metadata' or 'history'.")
    return(SMRMonFailureSymbol)
  }

  if( is.numeric(outlierIdentifierParameters) && length(outlierIdentifierParameters) == 2 ) {

    oiFunc <- function(x) OutlierIdentifiers::TopOutlierIdentifier( x, lowerAndUpperThresholds = outlierIdentifierParameters )

  } else if( is.function(outlierIdentifierParameters) ) {

    oiFunc <- function(x) OutlierIdentifiers::TopOutlierIdentifier( x, identifier = outlierIdentifierParameters )

  } else if( !is.null(outlierIdentifierParameters) ) {

    warning( "The argument is outlierIdentifierParameters is expected to be NULL, a pair of numbers, or a function.", call. = TRUE)
    return(SMRMonFailureSymbol)

  } else {
    oiFunc <- NULL
  }

  if( is.function(oiFunc) ) {

    res <- res[ oiFunc(res$Score), ]

    if( is.null(res) || nrow(res) == 0 ) {
      warning( paste( "An empty data frame of", type, "proofs was obtained after applying the outlier identification",
                      "for the item(s):", paste(items, collapse = ","), "." ), call. = TRUE )
    }
  }

  smrObj$Value <- res

  smrObj
}


##===========================================================
## Prove by metadata
##===========================================================

#' Prove a recommendation to a profile using metadata.
#' @description For a given profile and item finds scored metadata tags
#' that appear in both the given profile and the profile of the given item.
#' @param smrObj An SMRMon object.
#' @param profile A profile specification.
#' @param items One or more item-indexes or item-names to make proofs for.
#' Each element must be a row index or row name of \code{smrObj$M}.
#' @param normalizeScoresQ Should the proof scores be normalized or not?
#' @param style Proof style derivation; one of "intersection", "multiplication".
#' @param outlierIdentifierParameters Outlier identifier parameters or parameters finding function.
#' If NULL all tags are returned.
#' @param warningQ Should warnings be given?
#' @details The result is a data frame with columns names "Score", "Index", "Tag"
#' and it is assigned to \code{smrObj$Value}.
#' @return An SMRMon object.
#' @family Recommendations computation functions
#' @export
SMRMonProveByMetadata <- function( smrObj, profile, items, normalizeScoresQ = TRUE, style = "intersection", outlierIdentifierParameters = NULL, warningQ = TRUE ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  SMRMonProve( smrObj = smrObj, spec = profile, items = items, type = "metadata",
               normalizeScoresQ = normalizeScoresQ, style = style, outlierIdentifierParameters = outlierIdentifierParameters, warningQ = warningQ )
}


##===========================================================
## Prove by history
##===========================================================

#' Prove a recommendation to a history.
#' @description Finds the items of the history that are the closest to a recommendation.
#' @param smrObj An SMRMon object.
#' @param history A history specification.
#' @param items One or more item-indexes or item-names to make proofs for.
#' Each element must be a row index or row name of \code{smrObj$M}.
#' @param normalizeScoresQ Should the proof scores be normalized or not?
#' @param outlierIdentifierParameters Outlier identifier parameters or parameters finding function.
#' If NULL all items are returned.
#' @param warningQ Should warnings be given?
#' @details The result is a data frame with columns names
#' "Score", "Index", and \code{smrObj$ItemColumnName}
#' and it is assigned to \code{smrObj$Value}.
#' @return An SMRMon object.
#' @family Recommendations computation functions
#' @export
SMRMonProveByHistory <- function( smrObj, history, items, normalizeScoresQ = TRUE, outlierIdentifierParameters = NULL, warningQ = TRUE ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }


  SMRMonProve( smrObj = smrObj, spec = history, items = items, type = "history",
               normalizeScoresQ = normalizeScoresQ, outlierIdentifierParameters = outlierIdentifierParameters, warningQ = warningQ )
}


##===========================================================
## Apply tag weights
##===========================================================

#' Apply tag weights
#' @description Multiplies the weights of tags of a sparse matrix recommender object.
#' @param smrObj A sparse matrix recommender.
#' @param weights A list/vector of weights to be applied.
#' @param default The weight to be used for weights not specified in weights.
#' @details
#' If \code{weights} does not have names it is going to replicated to match
#' the length of \code{ncol(smr$M)}.
#' If \code{weights} has names the missing tag types are (if any)
#' are set to have the value \code{default}.
#' @return An SMRMon object.
#' @export
SMRMonApplyTagWeights <- function( smrObj, weights, default = 1 ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  #default <- if ( is.null(weights$Default) ) { 0 } else { weights$Default }

  if( is.null(names(weights)) ) {
    weights <- setNames( c( weights, rep( default, ncol(smrObj$M) - length(weights) ) ), colnames(smrObj$M) )
  }

  cnames <- intersect( colnames(smrObj$M), names(weights) )

  if( length(cnames) == 0 ) {
    warning( "The tag types specified in weights are not known.", call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  diffnames <- setdiff( colnames(smrObj$M), cnames )

  weights <- c( weights[cnames], setNames( rep_len( x = default, length.out = length(diffnames) ), diffnames ) )

  weights <- weights[ colnames(smrObj$M) ]

  # SMRApplyTagWeights does multiplication with diagonal matrix made with weights as a diagonal.
  smrObj$M <- SMRApplyTagWeights( smr = smrObj, weights = weights )

  smrObj
}


##===========================================================
## Apply tag type weights
##===========================================================

#' Apply tag type weights
#' @description Multiplies the weights of tag types of a sparse matrix recommender object.
#' @param smrObj A sparse matrix recommender.
#' @param weights A list/vector of weights to be applied.
#' @param default The weight to be used for weights not specified in weights.
#' @details
#' If \code{weights} does not have names it is going to replicated to match
#' the length of \code{smr$TagTypes}.
#' If \code{weights} has names the missing tag types are (if any)
#' are set to have the value \code{default}.
#' @return An SMRMon object.
#' @export
SMRMonApplyTagTypeWeights <- function( smrObj, weights, default = 1 ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  #default <- if ( is.null(weights$Default) ) { 0 } else { weights$Default }

  if( is.null(names(weights)) ) {
    weights <- setNames( c( weights, rep( default, length(smrObj$TagTypes) - length(weights) ) ), smrObj$TagTypes )
  }

  cnames <- intersect( smrObj$TagTypes, names(weights) )

  if( length(cnames) == 0 ) {
    warning( "The tag types specified in weights are not known.", call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  diffnames <- setdiff( smrObj$TagTypes, cnames )

  weights <- c( weights[cnames], setNames( rep_len( x = default, length.out = length(diffnames) ), diffnames ) )

  weights <- weights[ smrObj$TagTypes ]

  smrObj$M <- SMRApplyTagTypeWeights( smr = smrObj, weights = weights )

  smrObj
}


##===========================================================
## Filter by profile
##===========================================================

#' Filter by profile
#' @description Applies a profile filter to the rows of the recommendation matrix.
#' @param smrObj A sparse matrix recommender.
#' @param profile A profile specification used to filter with.
#' @param type The type of filtering one of "union" or "intersection".
#' @details
#' The result is a vector of scored items that is assigned to \code{smrObj$Value}.
#' If \code{type} is "union" each item that has at least one of the tags in
#' \code{profile} is in the result.
#' (Essentially, that is the same as taking all non-zero score recommendations by profile.)
#' If \code{type} is "intersection" each item in the result
#' has all tags in \code{profile}.
#' @return An SMRMon object.
#' @export
SMRMonFilterByProfile <- function( smrObj, profile, type = "intersection" ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  smrObj <- SMRMonGetProfileDataFrame( smrObj = smrObj, profile = profile, functionName = "SMRMonFilterByProfile", warningQ = TRUE )
  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  profile <- smrObj %>% SMRMonTakeValue

  profileVec <- SMRProfileDFToVector( smr = smrObj, profileDF = profile )

  ## Unitize
  profileVec@x[ profileVec@x > 0 ] <- 1

  ## Find the items corresponding to the profile and type spec
  if( is.character(type) && tolower(type) == "union" ) {

    svec <- (smrObj %>% SMRMonTakeM) %*% profileVec

  } else if ( is.character(type) && tolower(type) == "intersection" ) {

    smat <- (smrObj %>% SMRMonTakeM)
    smat@x[ smat@x > 0 ] <- 1

    svec <- smat %*% profileVec

    svec@x[ svec@x < nrow(profile) ] <- 0

  } else {

    warning("The argument type is expected to be one of 'union' or 'intersection'.", call. = TRUE)
    return(SMRMonFailureSymbol)

  }

  ## The result
  svec <- svec[ svec[,1] > 0, 1]
  smrObj$Value <- data.frame( Score = svec, Item = names(svec), stringsAsFactors = FALSE )
  smrObj$Value <- setNames( smrObj$Value, c("Score", smrObj %>% SMRMonTakeItemColumnName) )

  smrObj
}


##===========================================================
## Filter matrix
##===========================================================

#' Filter recommendation matrix rows
#' @description Applies a profile filter to the rows of the recommendation matrix.
#' @param smrObj A sparse matrix recommender.
#' @param profile A profile specification used to filter with.
#' @param type The type of filtering one of "union" or "intersection".
#' @details
#' If \code{type} is "union" each item that has at least one of the tags in
#' \code{profile} is in the result recommender.
#' If \code{type} is "intersection" each item in the result recommender
#' has all tags in \code{profile}.
#' This function is based on \code{\link{SMRMonFilterByProfile}}.
#' @return An SMRMon object.
#' @export
SMRMonFilterMatrix <- function( smrObj, profile, type = "union" ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  svec <-
    smrObj %>%
    SMRMonFilterByProfile( profile = profile, type = type ) %>%
    SMRMonTakeValue

  if( SMRMonFailureQ(svec) ) { return(SMRMonFailureSymbol) }

  ## Make the recommender object
  smrObj <-
    smrObj %>%
    SMRMonSetM( (smrObj %>% SMRMonTakeM)[ svec[[ smrObj %>% SMRMonTakeItemColumnName ]], , drop=F ] ) %>%
    SMRMonSetM01( (smrObj %>% SMRMonTakeM01)[ svec[[ smrObj %>% SMRMonTakeItemColumnName ]], , drop=F ] )

  smrObj
}



##===========================================================
## Retrieve by query elements
##===========================================================

#' Retrieve by query elements
#' @description Applies a profile filter to the rows of the recommendation matrix.
#' @param smrObj A sparse matrix recommender.
#' @param should A profile specification used to recommend with.
#' @param must A profile specification used to filter with.
#' The items in the result must have the tags in \code{must}.
#' @param mustNot A profile specification used to filter with.
#' The items in the result must not have the tags in \code{mustNot}.
#' @details
#' The result is assigned to \code{smrObj$Value}.
#' This function is based on \code{\link{SMRMonRecommendByProfile}} ("should")
#' and \code{\link{SMRMonFilterByProfile}} ("must" and "must not").
#' @return An SMRMon object.
#' @export
SMRMonRetrieveByQueryElements <- function( smrObj, should = NULL, must = NULL, mustNot = NULL ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  ## Should
  if( !is.null(should) && !is.null(must) ) {

    pvecShould <- smrObj %>% SMRMonGetProfileVector(profile = should) %>% SMRMonTakeValue
    if( SMRMonFailureQ(pvecShould) ) { return(SMRMonFailureSymbol) }

    pvecMust <- smrObj %>% SMRMonGetProfileVector(profile = must) %>% SMRMonTakeValue
    if( SMRMonFailureQ(pvecMust) ) { return(SMRMonFailureSymbol) }

    shouldItems <-
      smrObj %>%
      SMRMonRecommendByProfile( profile = pvecShould + pvecMust, nrecs = NULL ) %>%
      SMRMonTakeValue

    if( SMRMonFailureQ(shouldItems) ) { return(SMRMonFailureSymbol) }

    shouldItems <- shouldItems[ , c("Score", smrObj %>% SMRMonTakeItemColumnName ) ]

  } else {

    shouldItems <-
      data.frame(
        Score = rowSums( smrObj %>% SMRMonTakeM ),
        Item = rownames( smrObj %>% SMRMonTakeM ),
        stringsAsFactors = FALSE
      )

    shouldItems <- setNames( shouldItems, c("Score", smrObj %>% SMRMonTakeItemColumnName) )

  }

  res <- shouldItems

  ## Must
  if( !is.null(must) ) {

    mustItems <-
      smrObj %>%
      SMRMonFilterByProfile( profile = must, type = "intersection" ) %>%
      SMRMonTakeValue

    if( SMRMonFailureQ(mustItems) ) { return(SMRMonFailureSymbol) }

  } else {
    mustItems <- NULL
  }

  if( !is.null(mustItems) ) {
    res <- res[ res[[ smrObj %>% SMRMonTakeItemColumnName ]] %in% mustItems[[ smrObj %>% SMRMonTakeItemColumnName ]], ]
  }

  ## Must not
  if( !is.null(mustNot) ) {

    mustNotItems <-
      smrObj %>%
      SMRMonFilterByProfile( profile = mustNot, type = "union" ) %>%
      SMRMonTakeValue

    if( SMRMonFailureQ(mustNotItems) ) { return(SMRMonFailureSymbol) }

  } else {
    mustNotItems <- NULL
  }

  if( !is.null(mustNotItems) ) {
    res <- res[ !( res[[ smrObj %>% SMRMonTakeItemColumnName ]] %in% mustNotItems[[ smrObj %>% SMRMonTakeItemColumnName ]] ), ]
  }

  ## Result
  smrObj$Value <- res

  smrObj
}


##===========================================================
## Make tag type recommender
##===========================================================

#' Make tag type recommender
#' @description Converts the recommender into a recommender for one of the tag types.
#' @param smrObj A sparse matrix recommender.
#' @param tagTypeTo Tag type to make a recommender for.
#' @param nTopTags Number of top tags from \code{tagTypeTo} when making item-tag
#' replacements.
#' @param tagTypes 	A vector tag types (strings) to make the data frame with.
#' If NULL all tag types are used. Passed to \code{\link{SMRMatricesToLongForm}}.
#' @param tagSelectionCriteria Tag selection criteria.
#' If a positive integer for each tag type the number of \code{tagSelectionCriteria} top tags are taken.
#' If a function that function is expected to give a list of booleans for a given list of tag weights.
#' The tags with weights that correspond to TRUE are selected.
#' @param ... Additional arguments for \code{\link{SMRMatricesToLongForm}} or \code{\link{SMRCreateFromMatrices}}.
#' @return A sparse matrix recommender
#' @details
#' This monad function calls the function \code{\link{SMRToMetadataRecommender}}.
#' The following steps are taken.
#' (1) The long form of the recommender is made.
#' (2) The items are replaced with the top tags of \code{tagTypeTo}.
#' (3) A new recommender is created with items that are the tags of \code{tagTypeTo}.
#' @export
SMRMonMakeTagTypeRecommender <- function( smrObj, tagTypeTo, nTopTags = 1, tagTypes = NULL, tagSelectionCriteria = NULL, ...) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }


  smrTagTypeObj <-
    tryCatch(

      expr = {
        smrTagTypeObj <- SMRToMetadataRecommender( smr = smrObj, tagTypeTo = tagTypeTo, nTopTags = nTopTags, tagTypes = tagTypes, tagSelectionCriteria = tagSelectionCriteria, ... )
      },

      error = function(e) {
        message( "Error attempting to run SMRToMetadataRecommender." )
        print(e)
        return(NULL)
      },

      warning = function(w) {
        message( "Warning attempting to run SMRToMetadataRecommender." )
        print(w)
        return(NULL)
      },

      finally = { }
    )

  if( is.null(smrTagTypeObj) ) {

    warning("Failure while running SMRMonMakeTagTypeRecommender.")

    return(SMRMonFailureSymbol)

  } else if( SMRSparseMatrixRecommenderQ(smrTagTypeObj) ) {

    return(smrTagTypeObj)

  }

}


##===========================================================
## Tag nearest neighbors
##===========================================================

#' Tag nearest neighbors
#' @description Find nearest neighbors for a given vector of tags.
#' @param smrObj A sparse matrix recommender.
#' @param tags Tags for which nearest neighbors are found.
#' @param tagType The tag type of the nearest neighbors.
#' @param nrecs Number of nearest neighbors.
#' @param nrecsProfile Number of recommendations for finding the \code{tags} profile.
#' @param ... Additional arguments passed to \code{\link{SMRClassifyByProfileVector}}.
#' @details The result is a list of scored tags that is assigned
#' to \code{smrObj$Value}.
#' This function is based in \code{\link{SMRClassifyByProfileVector}}.
#' The tags correspond to columns of the SMR object sparse matrix.
#' (The columns of that matrix are assumed to be unique.)
#' @return An SMRMon object.
#' @export
SMRMonTagNearestNeighbors <- function( smrObj, tags, tagType, nrecs = 12, nrecsProfile = 100, ...) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  prof <- data.frame( Score = 1, Tag = tags, stringsAsFactors = FALSE)

  recs <- SMRRecommendationsByProfileDF( smr = smrObj, profile = prof, nrecs = nrecsProfile )

  prof <- SMRProfileDF( smr = smrObj, itemHistory = setNames( recs[, c("Score", smrObj$ItemColumnName) ], c( "Rating", smrObj$ItemColumnName) ) )
  profVec <- SMRProfileDFToVector( smr = smrObj, profileDF = prof )

  res <- SMRClassifyByProfileVector( smr = smrObj, tagType = tagType, profileVec = profVec, nTopNNs = nrecsProfile, ... )

  smrObj$Value <- res

  smrObj
}


##===========================================================
## Compute Top-K statistic
##===========================================================

#' Top-K statistic computation
#' @description Computes the Top-K statistic for a data frame with the scored similarity pairs.
#' @param smrObj A sparse matrix recommender.
#' @param testData A data frame with columns \code{c( "Score", "SearchID", SMRMonTakeItemColumnName(smrObj) )}.
#' @param ks An integer vector with k-values for the Top-K statistic.
#' @param type One of "fraction", "count", "incidence".
#' @param ... Additional arguments passed to \code{\link{SMRMonRecommend}}.
#' @details The computation result is assigned to \code{smrObj$Value}.
#' @return An SMRMon object.
#' @export
SMRMonComputeTopK <- function( smrObj, testData, ks, type = "fraction", ...) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !( is.data.frame(testData) &&
         ( sum( colnames(testData) %in% c( "Score", "SearchID", smrObj %>% SMRMonTakeItemColumnName ) ) == 3 ||
           sum( colnames(testData) %in% c( "SearchID", smrObj %>% SMRMonTakeItemColumnName ) ) == 2 ) ) ) {
    warning( paste( "The argument testData is expected to be a data frame with column names",
                    "c( 'Score', 'SearchID',", smrObj %>% SMRMonTakeItemColumnName, ") or",
                    "c( 'SearchID',", smrObj %>% SMRMonTakeItemColumnName, ") ." ), call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  if( ! is.numeric(ks) ) {
    warning( "The argument ks is expected to be an integer vector.", call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  expectedTypes <- c( "fraction", "count", "incidence", "binary" )
  if( ! ( tolower(type) %in% expectedTypes ) ) {
    warning( paste( "The argument type is expected to be one of: ", paste(expectedTypes[-length(expectedTypes)], collapse = ", "), "." ), call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  ## Sorting ks.
  ks <- sort(ks)

  res <-
    purrr::map_df( split(testData, testData$SearchID ), function(df) {

      if( "Score" %in% colnames(df) ) {
        df <- df[ order(-df$Score), ]
      }

      kRecs <- smrObj %>% SMRMonRecommend( history = df$SearchID[1], nrecs = max(ks), ... ) %>% SMRMonTakeValue

      if( type == "fraction" ) {

        topKStat <-
          purrr::map_dbl( ks, function(k) {
            mean( df[[smrObj$ItemColumnName]] %in% kRecs[[smrObj$ItemColumnName]][1:k] )
          })

      } else if (type == "count") {

        topKStat <-
          purrr::map_dbl( ks, function(k) {
            sum( df[[smrObj$ItemColumnName]] %in% kRecs[[smrObj$ItemColumnName]][1:k] )
          })

      } else if (type %in% c( "binary", "incidence") ) {

        topKStat <-
          purrr::map_dbl( ks, function(k) {
            sum( df[[smrObj$ItemColumnName]] %in% kRecs[[smrObj$ItemColumnName]][1:k] ) > 0
          })

      } else {
        # Should not happen.
        return(SMRMonFailureSymbol)
      }

      data.frame( SearchID = df$SearchID[1], K = ks, Value = topKStat, N = nrow(df), stringsAsFactors = FALSE)
    })

  smrObj$Value <- res

  smrObj
}



##===========================================================
## SMRMonRetrievalByProfileStatistics
##===========================================================

#' Retrieval by profile statistics
#' @description Computes the Top-K statistics between the items that have a given focus tag
#' and the profile recommendations for that focus tag (as a profile.)
#' @param smrObj A sparse matrix recommender.
#' @param focusTag The tag to be tested.
#' @param focusTagType The tag type of the tag to be tested.
#' @param profileTagTypes A character vector of tag types.
#' @param nrecs Number of recommendations.
#' @details The computation result is assigned to \code{smrObj$Value}.
#' The computation steps follow.
#' (1) Find all items that have \code{focustTag}.
#' (2) Find the profile of the focus tag items.
#' (3) Find recommendations with the profile according to \code{max(nrecs)}.
#' (4) Find how many focus tag items (of step 1) are present in the recommendations (of step 3).
#' @return An SMRMon object.
#' @export
SMRMonRetrievalByProfileStatistics <- function( smrObj, focusTag, focusTagType, profileTagTypes, nrecs = 100 ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( ! ( is.character(focusTag) && length(focusTag) == 1 ) ) {
    warning( "The argument focusTag is expected to be a string (a character vector of length 1.)", call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  if( ! ( is.character(focusTagType) && length(focusTagType) == 1 ) ) {
    warning( "The argument focusTagType is expected to be a string (a character vector of length 1.)", call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  if( !is.character(profileTagTypes) ) {
    warning( "The argument profileTagTypes is expected to be a character vector.", call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  if( !( is.numeric(nrecs) && nrecs > 0 ) ) {
    warning( "The argument nrecs is expected to be a positive integer.", call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  if( !( focusTagType %in% (smrObj %>% SMRMonTakeTagTypes) ) ) {
    warning( "The value of focusTagType is not known by the recommender object smrObj.", call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  if( mean( profileTagTypes %in% (smrObj %>% SMRMonTakeTagTypes) ) < 1 ) {
    warning( "At least one of profileTagTypes is not known by the recommender object smrObj.", call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  ## Get the items corresponding to a tag.
  dfTagItems <-
    smrObj %>%
    SMRMonRecommendByProfile( profile = focusTag, nrecs = NULL ) %>%
    SMRMonTakeValue

  ## Find the profile of the tag items.
  ## Take profile tags that belong to the specified tag types.
  dfTagProfile <-
    smrObj %>%
    SMRMonProfile( history = dfTagItems[[ smrObj %>% SMRMonTakeItemColumnName ]], tagTypesQ = TRUE ) %>%
    SMRMonTakeValue %>%
    dplyr::filter( TagType %in% profileTagTypes )

  ## Find recommendations with the tag profile.
  dfRecs <-
    smrObj %>%
    SMRMonRecommendByProfile( profile = dfTagProfile, nrecs = max(nrecs) ) %>%
    SMRMonTakeValue

  ## Get the sub-matrix for the focus tag type.
  smat <- SMRSubMatrix( smrObj, tagType = focusTagType )

  dfRecs <- dfRecs[ dfRecs[[ smrObj %>% SMRMonTakeItemColumnName ]] %in% rownames(smat), ]

  if( nrow(dfRecs) == 0 ) {
    warning( paste0( "No recommendations were found for tag: '", focusTag, "'."), call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  ## Compute Top-K statistics.
  res <-
    purrr::map_df( nrecs, function(k) {

      smat <- smat[ dfRecs[[ smrObj %>% SMRMonTakeItemColumnName ]][ 1:min(k, nrow(dfRecs) ) ], ]
      dfRecTags <- SMRSparseMatrixToTriplets(smat)

      data.frame( Tag = focusTag,
                  Mean = sum( dfRecTags$j == focusTag ) / min(nrow(dfTagItems), k),
                  TopKMean = mean( dfRecTags$j == focusTag ),
                  Count = sum( dfRecTags$j == focusTag ),
                  K = k,
                  N = nrow(dfTagItems),
                  stringsAsFactors = FALSE )
    })

  smrObj$Value <- res

  smrObj
}


##===========================================================
## Find anomalies by nearest neighbors
##===========================================================

#' Find anomalies by nearest neighbors distances
#' @description Find anomalies by nearest neighbors distances distributions.
#' @param smrObj A sparse matrix recommender.
#' @param data Data vectors to be evaluated as anomalous or not.
#' If NULL the \code{smrObj$M} is used.
#' @param numberOfNearestNeighbors Number of nearest neighbors.
#' @param aggregationFunction Aggregation function for the nearest neighbor distances.
#' If NULL \code{mean} is used.
#' @param thresholdsIdentifier Outlier thresholds identifier.
#' If NULL then
#' \code{function(x) OutlierIdentifiers::BottomOutliersOnlyThresholds( OutlierIdentifiers::HampelIdentifierParameters(x) )}
#' is used.
#' @param normalizeQ Should each recommendation scores be normalized or not?
#' @param property A string.
#' One of \code{c("Similarities", "SparseMatrix", "RowNames", "OutlierThresholds", "Properties")}
#' @param useBatchRecommendationQ Should batch recommendation be used or not?
#' @details The computation result is assigned to \code{smrObj$Value}.
#' The computation steps follow.
#' (1) For each row of the given data find the specified number of Nearest Neighbors (NNs) in the recommender matrix.
#' (2) For each row aggregate the NNs scores with the specified aggregation function.
#' (3) Find outlier thresholds for the list of aggregated values.
#' (4) Identify the outliers by using the outlier thresholds.
#' @return An SMRMon object.
#' @export
SMRMonFindAnomalies <- function( smrObj,
                                 data = NULL,
                                 numberOfNearestNeighbors = 12,
                                 aggregationFunction = mean,
                                 thresholdsIdentifier = NULL,
                                 normalizeQ = FALSE,
                                 property = "RowNames",
                                 useBatchRecommendationQ = TRUE ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "M", memberPrettyName = "M", functionName = "SMRMonFindAnomalies",  logicalResultQ = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  ## Processing data
  if( ! ( is.null(data) || SMRSparseMatrixQ(data) && ncol(data) == ncol(smrObj$M) ) ) {
    warning( paste( "The argument data is expected to be NULL or a matrix or sparse matrix with the same number of columns as smrObj$M,", ncol(smrObj$M), "."), call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  ## Processing numberOfNearestNeighbors
  if( is.null(numberOfNearestNeighbors) ) { numberOfNearestNeighbors <- 12 }

  if( ! ( is.numeric(numberOfNearestNeighbors) && length(numberOfNearestNeighbors) == 1 && numberOfNearestNeighbors > 0 ) ) {
    warning( "The argument numberOfNearestNeighbors is expected to be a positive number or NULL.", call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  ## Processing aggregationFunction
  if( is.null(aggregationFunction) ) { aggregationFunction <- mean }

  if( ! ( is.function(aggregationFunction) ) ) {
    warning( "The argument aggregationFunction is expected to be a function or NULL", call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  ## Processing thresholdsIdentifier
  if( is.null(thresholdsIdentifier) ) {
    thresholdsIdentifier <- function(x) OutlierIdentifiers::BottomOutliersOnlyThresholds(OutlierIdentifiers::HampelIdentifierParameters(x))
  }

  if( ! ( is.function(thresholdsIdentifier) ) ) {
    warning( "The argument thresholdsIdentifier is expected to be a function or NULL", call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  ## Processing property
  lsKnownProperties <- c("Similarities", "SparseMatrix", "Indices", "RowNames", "OutlierThresholds", "Properties")
  if( !( is.character(property) && length(property) == 1 && ( tolower(property) %in% tolower(lsKnownProperties) ) ) ) {
    warning( paste( "The argument property is expected to be a string, one of: ", paste( lsKnownProperties, collapse = ", "), "." ), call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  property <- tolower(property)
  if( property == "properties" ) {
    smrObj$Value <- lsKnownProperties
    return(smrObj)
  }

  ## Compute the Cosine/Dot distances to the rows of monad's matrix.
  ## (This first version can be sped-up using matrix products.)

  if( useBatchRecommendationQ ) {

    # We have to use numberOfNearestNeighbors + 1 because later on we always remove the self-similarity rows.
    dfNNs <-
      smrObj %>%
      SMRMonBatchRecommend( data = data, nrecs = numberOfNearestNeighbors + 1, removeHistoryQ = FALSE, normalizeQ = FALSE, targetColumnName = "SearchID" ) %>%
      SMRMonTakeValue

    if( SMRMonFailureQ(dfNNs) ) { return(SMRMonFailureSymbol) }

  } else {

    if( is.null(data) ) {

      # We have to use numberOfNearestNeighbors + 1 because later on we always remove the self-similarity rows.
      dfNNs <-
        purrr::map_df( rownames(smrObj$M), function(sid) {

          dfRes <-
            smrObj %>%
            SMRMonRecommend( history = sid, nrecs = numberOfNearestNeighbors + 1, removeHistoryQ = FALSE, normalizeQ = FALSE ) %>%
            SMRMonTakeValue

          if( SMRMonFailureQ(dfRes) ) { return(SMRMonFailureSymbol) }

          cbind( SearchID = sid, dfRes, stringsAsFactors = FALSE)
        } )

    } else {

      if( is.null(rownames(data)) ) { rownames(data) <- as.character(1:ncol(data)) }

      dfNNs <-
        purrr::map_df( rownames(data), function(sid) {

          dfRes <-
            smrObj %>%
            SMRMonRecommendByProfile( profile = data[sid, , drop=F], nrecs = numberOfNearestNeighbors, normalizeQ = FALSE ) %>%
            SMRMonTakeValue

          if( SMRMonFailureQ(dfRes) ) { return(SMRMonFailureSymbol) }

          cbind( SearchID = sid, dfRes, stringsAsFactors = FALSE)
        } )

    }
  }

  ## Normalize
  if( normalizeQ ) {
    dfNNs <-
      dfNNs %>%
      dplyr::group_by( SearchID ) %>%
      dplyr::mutate( MaxScore = max(abs(Score)) ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate( Score = ifelse( MaxScore > 0, Score / MaxScore, Score ) ) %>%
      dplyr::mutate( MaxScore = NULL )
  }

  ## Remove history
  dfNNs <- dfNNs[ dfNNs$SearchID != dfNNs[[ smrObj %>% SMRMonTakeItemColumnName ]], ]

  ## Aggregate
  dfAggrVals <-
    dfNNs %>%
    dplyr::group_by( SearchID ) %>%
    dplyr::summarise( AValue = aggregationFunction(Score), .groups = "drop_last" )

  ## Find the outlier thresholds
  lsOutlierThresholds <- thresholdsIdentifier( dfAggrVals$AValue )

  ## Assign outlier or not predicate column.
  dfAggrVals <-
    cbind( dfAggrVals,
           OutlierQ = OutlierIdentifiers::OutlierIdentifier( data = dfAggrVals$AValue, lowerAndUpperThresholds = lsOutlierThresholds ),
           stringsAsFactors = FALSE )

  ## Return result according to the property argument.
  if( property %in% tolower(c( "Similarities", "AggregatedSimilarities" )) ) {

    smrObj$Value <- dfNNs

  } else if( property %in% tolower(c("Indices", "Indexes")) ) {

    ## This assumes that dplyr preserved the order after using group_by and summarise
    ## smrObj$Value <- 1:nrow(dfAggrVals)[ dfAggrVals$OutlierQ ]

    smrObj$Value <- dfAggrVals[ dfAggrVals$OutlierQ, ]$SearchID

    if( is.null(data) ) {
      smrObj$Value <- which( rownames(smrObj$M) %in% smrObj$Value )
    } else {
      smrObj$Value <- which( rownames(data) %in% smrObj$Value )
    }

  } else if( property %in% tolower(c("RowNames")) ) {

    smrObj$Value <- dfAggrVals[ dfAggrVals$OutlierQ, ]$SearchID

  } else if( property %in% tolower(c("Thresholds", "OutlierThresholds")) ) {

    smrObj$Value <- lsOutlierThresholds

  } else if( property %in% tolower(c("Matrix", "SparseMatrix")) ) {

    smrObj$Value <- smrObj$M[  dfAggrVals[ dfAggrVals$OutlierQ, , drop = F]$SearchID, , drop=F]

  } else {

    warning( paste( "The argument property is expected to be a string, one of: ", paste( lsKnownProperties, collapse = ", "), "." ), call. = TRUE )
    return(SMRMonFailureSymbol)

  }

  smrObj

}



