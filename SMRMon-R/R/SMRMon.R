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
# antononcube @ gmail . com,
# Windermere, Florida, USA.
#
#=======================================================================================
#
# This software monad is a "monadic interface" to the packages
# SparseMatrixRecommender and SparseMatrixRecommenderInterfaces. See:
#   https://github.com/antononcube/R-packages/tree/master/SparseMatrixRecommender ,
#   https://github.com/antononcube/R-packages/tree/master/SparseMatrixRecommenderInterfaces .
#
#=======================================================================================


#' @import SparseMatrixRecommender
#' @import SparseMatrixRecommenderInterfaces
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
    if ( tolower(property) %in% tolower( c( "tags", "columns" ) ) ) {
      colnames(smat)
    } else if( tolower(property) %in% tolower( c( "rows") ) ) {
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
#' @details An S3 object is returned that is list with class attribute set to "SMR".
#' @return SMR object.
#' @family Creation functions
#' @export
SMRMonCreate <- function( smrObj, data = SMRMonTakeData(smrObj), tagTypes = names(data)[-1], itemColumnName = names(data)[1] ) {

  ## We allow anything to be smrObj .
  ## if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  res <- SMRCreate( dataRows = data, tagTypes = tagTypes, itemColumnName = itemColumnName )

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
#' @details An S3 object is returned that is list with class attribute set to "SMR".
#' @return An SMRMon object.
#' @family Creation functions
#' @export
SMRMonCreateFromMatrices <- function( smrObj, matrices, tagTypes = names(matrices), itemColumnName = "Item" ) {

  ## We allow anything to be smrObj .
  ## if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  res <- SMRCreateFromMatrices( matrices = matrices, tagTypes = tagTypes, itemColumnName = itemColumnName )
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

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "M", memberPrettyName = "M", functionName = "SMRMonRecommend",  logicalResultQ = TRUE) ) {
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
#' @details The result data frame is assigned to \code{smrObj$Value}.
#' @return An SMRMon object.
#' @family Data functions
#' @export
SMRMonGetLongFormData <- function( smrObj, items = NULL, tagTypesQ = TRUE ) {

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

  ## Returned result
  smrObj$Value <- res %>% dplyr::arrange_at( .vars = smatColNames )

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
#' @details
#' The result is a list with named elements assigned to \code{smrObj$Value}.
#' @return A SMRMon object
#' @export
SMRMonSummarizeItem <- function( smrObj, item ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( is.null(item) ) {
    item <- smrObj %>% SMRMonTakeValue
  }

  if( !( is.character(item) && (item %in% rownames(smrObj$M) ) ) ) {
    warning( "The value of the argument item is expected to be one of rownames(smrObj$M).", call. = TRUE )
    return(SMRMonFailureSymbol)
  }

  ## Tags profile.
  dfProfile <-
    smrObj %>%
    SMRMonGetLongFormData( items = c(item), tagTypesQ = TRUE ) %>%
    SMRMonTakeValue

  ## Tags summary: number of tags, top tags/outliers.
  dfTagsSummary <-
    dfProfile %>%
    dplyr::group_by_at( .vars = c( smrObj %>% SMRMonTakeItemColumnName, "TagType" ) ) %>%
    dplyr::arrange( desc(Value) ) %>%
    dplyr::summarize( NumberOfTags = length(Tag) )

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
#' @param warningQ Should a warning be issued if \code{profile} is of unknown type?
#' @details The result data frame is with columns "Score", \code{smr$ItemColumnName};
#' assigned to \code{smrObj$Value}.
#' If \code{profile = NULL} then \code{smrObj$Value} is used.
#' @return A SMRMon object
#' @export
SMRMonGetProfileDataFrame <- function( smrObj, profile, functionName = "SMRMonGetProfileDataFrame", warningQ = TRUE ) {

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

  smrObj$Value <- data.frame( Score = profileScores, Tag = profileTags, stringsAsFactors = FALSE )

  smrObj
}


##===========================================================
## Get profile data frame
##===========================================================

is.sparseMatrix <- function(x) is(x, 'sparseMatrix')

#' From a profile specification into a vector
#' @description Transforms a profile specification into a sparse matrix
#' with one row.
#' @param smrObj An SMRMon object.
#' @param profile Profile specification.
#' A data frame with columns \code{c("Score", "Tag")};
#' a numeric vector named elements, the names being items;
#' a character vector, the correspond ratings assumed all to be 1.
#' @param functionName A string that is a name of this function or a delegating function.
#' @param warningQ Should a warning be issued if \code{profile} is of unknown type?
#' @details The result profile vector (sparse matrix) is
#' assigned to \code{smrObj$Value}.
#' If \code{profile = NULL} then \code{smrObj$Value} is used.
#' @return A SMRMon object
#' @export
SMRMonGetProfileVector <- function( smrObj, profile, functionName = "SMRMonGetProfileVector", warningQ = TRUE ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( is.null(profile) ) {
    profile <- smrObj %>% SMRMonTakeValue
  }

  if( is.sparseMatrix( profile ) ) {

    if( !( "dgCMatrix" %in% class(profile)) ) {
      profile <- as( profile, "dgCMatrix" )
    }

    if( ncol(profile) != 1 ) {
      warning( paste0( "The profile vector given to the function ", functionName,
                       " is expected to be a sparse matrix with one column."),
               call. = TRUE )
      return(SMRMonFailureSymbol)
    }

    if( nrow(profile) != ncol(smrObj$M) ) {
      warning( paste0( "The profile vector given to the function ", functionName,
                       " is expected to be a sparse matrix with ", ncol(smrObj$M),
                       " rows."),
               call. = TRUE )
      return(SMRMonFailureSymbol)
    }

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
SMRMonGetTopRecommendations <- function( smrObj, spec = NULL, nrecs = 12 ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if ( is.null(spec) ) {

    res <- smrObj %>% SMRMonRecommend( smrObj %>% SMRMonTakeValue, nrecs = nrecs, warningQ = FALSE )

    if( !SMRMonFailureQ(res) ) { return(res) }

    res <- smrObj %>% SMRMonRecommendByProfile(  smrObj %>% SMRMonTakeValue, nrecs = nrecs, warningQ = FALSE )

    if( SMRMonFailureQ(res) ) {
      warning( "The monad object 'Value' is not a history or profile specification.", call. = TRUE )
    }

  } else {

    res <- smrObj %>% SMRMonRecommend( spec, nrecs = nrecs )

    if( !SMRMonFailureQ(res) ) { return(res) }

    res <- smrObj %>% SMRMonRecommendByProfile( spec, nrecs = nrecs )

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
#' @param warningQ Should a warning be issued if \code{history} is of unknown type?
#' @details The recommendations result is a
#' data frame with columns "Score", "Index", \code{smr$ItemColumnName};
#' assigned to \code{smrObj$Value}.
#' @return A SMRMon object
#' @family Recommendations computation functions
#' @export
SMRMonRecommend <- function( smrObj, history, nrecs = 12, removeHistoryQ = FALSE, warningQ = TRUE ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "M", memberPrettyName = "M", functionName = "SMRMonRecommend",  logicalResultQ = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  smrObj <- smrObj %>% SMRMonGetHistoryDataFrame( history = history, functionName = "SMRMonRecommend", warningQ = warningQ )

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  history <- smrObj %>% SMRMonTakeValue

  res <- SMRRecommendations( smr = smrObj, userHistoryItems = history$Item, userRatings = history$Rating, nrecs = nrecs, removeHistory = removeHistoryQ )

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
#' a numeric vector named elements, the names being items;
#' a character vector, the correspond ratings assumed all to be 1;
#' a sparse matrix with 1 column and a number of rows that equals \code{ncol(smrObj$M)}.
#' @param nrecs Number of recommendations to be returned.
#' @param warningQ Should a warning be issued if \code{profile} is of unknown type?
#' @details The recommendations result is a
#' data frame with columns "Score", "Index", \code{smr$ItemColumnName};
#' assigned to \code{smrObj$Value}.
#' @return A SMRMon object
#' @family Recommendations computation functions
#' @export
SMRMonRecommendByProfile <- function( smrObj, profile, nrecs = 12, warningQ = TRUE ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "M", memberPrettyName = "M", functionName = "SMRMonRecommendByProfile",  logicalResultQ = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  if( is.sparseMatrix(profile) ) {

    smrObj <- smrObj %>% SMRMonGetProfileVector( profile = profile, functionName = "SMRMonRecommendByProfile", warningQ = warningQ )

    if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

    profile <- smrObj %>% SMRMonTakeValue

    res <- SMRRecommendationsByProfileVector( smr = smrObj, profileVec = profile, nrecs = nrecs )

  } else {

    smrObj <- smrObj %>% SMRMonGetProfileDataFrame( profile = profile, functionName = "SMRMonRecommendByProfile", warningQ = warningQ )

    if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

    profile <- smrObj %>% SMRMonTakeValue

    res <- SMRRecommendationsByProfileDF( smr = smrObj,
                                          profile = data.frame( Score = profile$Score, Tag = profile$Tag, stringsAsFactors = FALSE ),
                                          nrecs = nrecs )

  }

  smrObj$Value <- res

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

  if( "dgCMatrix" %in% class(profile)  ) {

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

    profile = data.frame( Score = profileScores, Tag = profileTags, stringsAsFactors = FALSE )

    profileVec <- SMRProfileDFToVector( smr = smrObj, profileDF = profile )
  }

  clRes <- SMRClassifyByProfileVector( smr = smrObj, tagType = tagType, profileVec = profileVec,
                                       nTopNNs = nTopNNs,
                                       voting = voting, dropZeroScoredLabels = dropZeroScoredLabels, maxNumberOfLabels = maxNumberOfLabels, normalizeQ = normalizeQ )

  smrObj$Value <- clRes

  smrObj
}


##===========================================================
## Apply tag type weights
##===========================================================

#' Apply tag type weights
#' @description Applies the weights of tag types of a sparse matrix recommender object.
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
## Filter matrix
##===========================================================

#' Filter recommendation matrix rows
#' @description Applies a profile filter to the rows of the recommendation matrix.
#' @param smrObj A sparse matrix recommender.
#' @param profile A profile specification used to filter with.
#' @details The matrix can be recovered with tag type
#' weights application, see \code{\link{SMRMonApplyTagTypeWeights}}.
#' @return An SMRMon object.
#' @export
SMRMonFilterMatrix <- function( smrObj, profile ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  smrObj <- SMRMonGetProfileDataFrame( smrObj = smrObj, profile = profile, functionName = "SMRMonFilterMatrix", warningQ = TRUE )
  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  profile <- smrObj %>% SMRMonTakeValue

  profileVec <- SMRProfileDFToVector( smr = smrObj, profileDF = profile )

  svec <- (smrObj %>% SMRMonTakeM) %*% profileVec

  smrObj <-
    smrObj %>%
    SMRMonSetM( (smrObj %>% SMRMonTakeM)[ svec[,1] > 0, ] ) %>%
    SMRMonSetM01( (smrObj %>% SMRMonTakeM01)[ svec[,1] > 0, ] )

  smrObj
}


##===========================================================
## Tag nerest neighbors
##===========================================================

#' Tag nearest neighbors
#' @description Find nearest neighbors for a given vector of tags.
#' @param smrObj A sparse matrix recommender.
#' @param tags Tags for which nearest neighbors are found.
#' @param tagType The tag type of the nearest neighbors.
#' @param nrecs Number of nearest neighbors.
#' @param nrecsProfile Number of recommendations for finding the \code{tags} profile.
#' @param normalizeQ Should the scores be normalized?
#' (By dividing by the maximum score.)
#' @param ... Additional arguments passed to \code{\link{SMRClassifyByProfileVector}}.
#' @details The result is a list of scored tags that is assigned
#' to \code{smrObj$Value}.
#' This function is based in \code{\link{SMRClassifyByProfileVector}}.
#' The tags to correspond to columns of the SMR object sparse matrix.
#' (The columns of that matrix assumed to be unique.)
#' @return An SMRMon object.
#' @export
SMRMonTagNearestNeighbors <- function( smrObj, tags, tagType, nrecs = 12, nrecsProfile = 100, normalizeQ, ...) {

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
#' @param ... Additional arguments passed to \code{\link{SMRMonRecommend}}.
#' @details The computation result is assigned to \code{smrObj$Value}.
#' @return An SMRMon object.
#' @export
SMRMonComputeTopK <- function( smrObj, testData, ks, ...) {

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
    warning( "The argument argument ks is expected to be an integer vector.", call. = TRUE )
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

      topKStat <-
        purrr::map_dbl( ks, function(k) {
          mean( df[[smrObj$ItemColumnName]] %in% kRecs[[smrObj$ItemColumnName]][1:k] )
        })

      data.frame( SearchID = df$SearchID[1], K = ks, Value = topKStat, stringsAsFactors = FALSE)
    })

  smrObj$Value <- res

  smrObj
}



