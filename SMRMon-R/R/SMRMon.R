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

  res <- list( Value = NULL, M = NULL, M01 = NULL, TagTypeRange = NULL, TagTypes = NULL, ItemColumnName = NULL, TagToIndexRules = NULL, ItemToIndexRules = NULL, Data = NULL )
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
#' @param logicalResult Should the result be logical value?
#' @return A logical value or an SMRMon object.
#' @export
SMRMonMemberPresenceCheck <- function( smrObj, memberName, memberPrettyName = memberName, functionName = "", logicalResult = FALSE ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  res <- TRUE

  if( nchar(functionName) > 0 ) { functionName <- paste0( functionName, ":: ") }

  if( is.null(smrObj[[memberName]]) ) {
    warning( paste0( functionName, paste0("Cannot find ", memberPrettyName, ".") ), call. = TRUE )
    res <- FALSE
  }

  if( logicalResult ) { res }
  else if ( !logicalResult && !res) { SMRMonFailureSymbol }
  else { smrObj }
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

  if( !( is.null(M) || is.matrix(M)) ) {
    warning("The argument M is expected to be NULL or a matrix.", call. = TRUE)
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

  if( !( is.null(M01) || is.matrix(M01)) ) {
    warning("The argument M01 is expected to be NULL or a matrix.", call. = TRUE)
    return(SMRMonFailureSymbol)
  }

  smrObj$M01 <- M01

  smrObj
}

##===========================================================
## TagTypeRange setter
##===========================================================

#' Set TagTypeRange.
#' @description Sets TagTypeRange into the monad object.
#' @param smrObj An SMRMon object.
#' @param TagTypeRange An object member to be set.
#' @return An SMRMon object.
#' @family Set/Take functions
#' @export
SMRMonSetTagTypeRange <- function( smrObj, TagTypeRange ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !( is.null(TagTypeRange) || is.data.frame(TagTypeRange)) ) {
    warning("The argument TagTypeRange is expected to be NULL or a data.frame.", call. = TRUE)
    return(SMRMonFailureSymbol)
  }

  smrObj$TagTypeRange <- TagTypeRange

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

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "M", memberPrettyName = "M", functionName = functionName,  logicalResult = TRUE) ) {
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

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "M01", memberPrettyName = "M01", functionName = functionName,  logicalResult = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  smrObj$M01
}

##===========================================================
## TagTypeRange Taker
##===========================================================

#' Take TagTypeRange.
#' @description Takes TagTypeRange from the monad object.
#' @param smrObj An SMRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{SMRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
SMRMonTakeTagTypeRange <- function( smrObj, functionName = "SMRMonTakeTagTypeRange" ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "TagTypeRange", memberPrettyName = "TagTypeRange", functionName = functionName,  logicalResult = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  smrObj$TagTypeRange
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

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "TagTypes", memberPrettyName = "TagTypes", functionName = functionName,  logicalResult = TRUE) ) {
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

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "ItemColumnName", memberPrettyName = "ItemColumnName", functionName = functionName,  logicalResult = TRUE) ) {
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

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "TagToIndexRules", memberPrettyName = "TagToIndexRules", functionName = functionName,  logicalResult = TRUE) ) {
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

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "ItemToIndexRules", memberPrettyName = "ItemToIndexRules", functionName = functionName,  logicalResult = TRUE) ) {
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

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "Data", memberPrettyName = "Data", functionName = functionName,  logicalResult = TRUE) ) {
    return(SMRMonFailureSymbol)
  }

  smrObj$Data
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
#' @param matrices A list of matrices to be spliced into a metadata matrix.
#' @param tagTypes Vector of matrix names.
#' @param itemColumnName The column name of recommender items (in data and recommendations).
#' @details An S3 object is returned that is list with class attribute set to "SMR".
#' @return SMR object.
#' @family Creation functions
#' @export
SMRMonCreateFromMatrices <- function( smrObj, matrices, tagTypes = names(matrices), itemColumnName = "Item" ) {

  res <- SMRCreateFromMatrices( matrices = matrices, tagTypes = tagTypes, itemColumnName = itemColumnName )

  res$Value <- NULL
  res$Data <- smrObj %>% SMRMonTakeData
  class(res) <- "SMR"

  res
}


##===========================================================
## Recommend by history
##===========================================================

#' Compute recommendations
#' @description Recommend items based on history of consumption.
#' @param smrObj An SMRMon object.
#' @param history (Rated) history items.
#' A data frame with columns \code{c("Rating", "Item")};
#' a numeric vector named elements, the names being items;
#' a character vector, the correspond ratings assumed all to be 1.
#' @param nrecs Number of recommendations to be returned.
#' @param removeHistoryQ Should the history be removed from the recommendations?
#' @return The recommendations result is a
#' data frame with columns "Score", "Index", \code{smr$ItemColumnName};
#' assigned to \code{smrObj$Value}.
#' @family Recommendations computation functions
#' @export
SMRMonRecommend <- function( smrObj, history, nrecs = 12, removeHistoryQ = FALSE ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "M", memberPrettyName = "M", functionName = "SMRMonRecommend",  logicalResult = TRUE) ) {
    return(SMRMonFailureSymbol)
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

    warning( "Unknown history type.", call. = TRUE )
    return(SMRMonFailureSymbol)

  }

  res <- SMRRecommendations( smr = smrObj, userHistoryItems = historyItems, userRatings = historyRatings, nrecs = nrecs )

  smrObj$Value <- res

  smrObj
}


##===========================================================
## Recommend by profile
##===========================================================

#' Recommendations using a profile
#' @description Recommend items based on a sparse matrix and a specified profile indices and scores.
#' @param smrObj An SMRMon object.
#' @param profile (Scored) profile tags.
#' A data frame with columns \code{c("Score", "Tag")};
#' a numeric vector named elements, the names being items;
#' a character vector, the correspond ratings assumed all to be 1.
#' @param nrecs Number of recommendations to be returned.
#' @return A data frame with columns "Score", "Index", "Item".
#' @family Recommendations computation functions
#' @export
SMRMonRecommendByProfile <- function( smrObj, profile, nrecs = 12 ) {

  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "M", memberPrettyName = "M", functionName = "SMRMonRecommendByProfile",  logicalResult = TRUE) ) {
    return(SMRMonFailureSymbol)
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

    warning( "Unknown profile type.", call. = TRUE )
    return(SMRMonFailureSymbol)

  }

  res <- SMRRecommendationsByProfileDF( smr = smrObj,
                                        profile = data.frame( Score = profileScores, Tag = profileTags, stringsAsFactors = FALSE ),
                                        nrecs = nrecs )

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
SMRMonClassifyByProfile <- function( smrObj, tagType, profile, nTopNNs,
                                     voting = FALSE, dropZeroScoredLabels = TRUE, maxNumberOfLabels = NULL, normalizeQ = TRUE ) {

  ## The following code shares a lot the beginning of SMRMonRecommendByProfile.
  if( SMRMonFailureQ(smrObj) ) { return(SMRMonFailureSymbol) }

  if( !SMRMonMemberPresenceCheck( smrObj, memberName = "M", memberPrettyName = "M", functionName = "SMRMonClassifyByProfile",  logicalResult = TRUE) ) {
    return(SMRMonFailureSymbol)
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

