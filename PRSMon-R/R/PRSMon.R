##===========================================================
## Proxy Recommendation System monad in R
## Copyright (C) 2020  Anton Antonov
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
## antononcube @@@ gmail ... com,
## Windermere, Florida, USA.
##===========================================================

#' @import dplyr
#' @import httr
#' @import jsonlite
#' @import magrittr
#' @import purrr
#' @import RCurl
NULL


##===========================================================
## PRSMon failure symbol
##===========================================================

#' Failure symbol for PRSMon.
#' @description Failure symbol for the monad PRSMon.
#' @export
PRSMonFailureSymbol <- NA

#' Failure test for an PRSMon object.
#' @description Test is an PRSMon object a failure symbol.
#' @export
PRSMonFailureQ <- function(x) { mean(is.na(x)) }


##===========================================================
## Failure function
##===========================================================

#' Failure producing function
#' @description Puts the monad object into a monadic failure.
#' @param prsObj An PRSMon object.
#' @param message A message to echoed. If NULL no message is echoed.
#' @return A PRSMon object.
#' @export
PRSMonFailure <- function( prsObj, message = NULL ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( is.character(message) ) {
    warning( message, call. = FALSE )
  }

  PRSMonFailureSymbol
}


##===========================================================
## PRSMon Unit
##===========================================================

#' Make a PRSMon Unit
#' @description Creates a monad object.
#' @return An S3 class "PRSMon". In other words, a list with the attribute "class" set to "PRSMon".
#' @export
PRSMonUnit <- function( ) {

  res <- list( Value = NULL, ServerSideObjectID = NULL, ServerURL = NULL, LastQuery = NULL, LastResult = NULL, DatabaseURL = NULL, Parser = NULL, ItemColumnName = NULL )
  attr(res, "class") <- "PRSMon"

  res
}


##===========================================================
## Value setter and getter
##===========================================================

#' Set the value in a PRSMon object.
#' @description Sets the value in a PRSMon monad object.
#' @param prsObj An PRSMon object.
#' @param value The new value.
#' @return A PRSMon object.
#' @details Assigns \code{value} to \code{prsObj$Value}.
#' @family Set/Take functions
#' @export
PRSMonSetValue <- function( prsObj, value ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  prsObj$Value <- value
  prsObj
}

#' Take the value in a PRSMon object.
#' @description Takes the value from PRSMon monad object.
#' @param prsObj An PRSMon object.
#' @return Just \code{prsObj$Value}.
#' @family Set/Take functions
#' @export
PRSMonTakeValue <- function( prsObj ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  prsObj$Value
}


##===========================================================
## Member presence check
##===========================================================

#' General member presence check.
#' @description A general function for checking the presence of a data member in an PRSMon object.
#' @param prsObj An PRSMon object.
#' @param memberName The name of the member to be checked.
#' @param memberPrettyName A pretty member name (for messages).
#' @param functionName The name of the delegating function.
#' @param logicalResultQ Should the result be a logical value?
#' @return A logical value or an PRSMon object.
#' @export
PRSMonMemberPresenceCheck <- function( prsObj, memberName, memberPrettyName = memberName, functionName = "", logicalResultQ = FALSE ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  res <- TRUE

  if( nchar(functionName) > 0 ) { functionName <- paste0( functionName, ":: ") }

  if( is.null(prsObj[[memberName]]) ) {
    warning( paste0( functionName, paste0("Cannot find ", memberPrettyName, ".") ), call. = TRUE )
    res <- FALSE
  }

  if( logicalResultQ ) { res }
  else if ( !logicalResultQ && !res) { PRSMonFailureSymbol }
  else { prsObj }
}


##===========================================================
## Echo monad's value
##===========================================================

#' Echo monad's value.
#' @description Prints the "Value" element/member of the monad object.
#' @param prsObj An PRSMon object.
#' @return A PRSMon object.
#' @details Prints \code{prsObj$Value}.
#' @export
PRSMonEchoValue <- function( prsObj ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  print( prsObj$Value )

  prsObj
}


##===========================================================
## Echo function application of over monad's value
##===========================================================

#' Echo function application to monad's value.
#' @description Applies a function to the "Value" element/member of the monad object
#' and prints the result.
#' @param prsObj An PRSMon object.
#' @param f A function to be applied to \code{prsObj$Value}.
#' @return A PRSMon object.
#' @details Prints \code{f(prsObj$Value)}.
#' @export
PRSMonEchoFunctionValue <- function( prsObj, f ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  print( f(prsObj$Value) )

  prsObj
}


##===========================================================
## Echo
##===========================================================

#' Echo.
#' @description Echoes given argument.
#' @param prsObj An PRSMon object.
#' @param x Object to be echoed.
#' @return An PRSMon object
#' @family Echo functions
#' @export
PRSMonEcho <- function( prsObj, x ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  print(x)

  prsObj
}


##===========================================================
## Optional function application over monad's object
##===========================================================

#' Optional function application to monad's object.
#' @description If monadic failure is obtained from \code{f(prsObj)}
#' then returns the original \code{prsObj};
#' else returns the result of \code{f(prsObj)}.
#' @param prsObj An PRSMon object.
#' @param f A function to be applied to the monad object.
#' @return A PRSMon object.
#' @details In general \code{f} should return a monad object,
#' but that is not enforced.
#' @export
PRSMonOption <- function( prsObj, f ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  res <- prsObj %>% f

  if( PRSMonFailureQ(res) ) { return(prsObj) }

  res
}

##===========================================================
## ServerSideObjectID setter
##===========================================================

#' Set ServerSideObjectID.
#' @description Sets ServerSideObjectID into the monad object.
#' @param prsObj An PRSMon object.
#' @param ServerSideObjectID An object member to be set.
#' @return An PRSMon object.
#' @family Set/Take functions
#' @export
PRSMonSetServerSideObjectID <- function( prsObj, ServerSideObjectID ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( !( is.null(ServerSideObjectID) || is.character(ServerSideObjectID)) ) {
    warning("The argument ServerSideObjectID is expected to be NULL or a character.", call. = TRUE)
    return(PRSMonFailureSymbol)
  }

  prsObj$ServerSideObjectID <- ServerSideObjectID

  prsObj
}

##===========================================================
## ServerURL setter
##===========================================================

#' Set ServerURL.
#' @description Sets ServerURL into the monad object.
#' @param prsObj An PRSMon object.
#' @param ServerURL An object member to be set.
#' @return An PRSMon object.
#' @family Set/Take functions
#' @export
PRSMonSetServerURL <- function( prsObj, ServerURL ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( !( is.null(ServerURL) || is.character(ServerURL)) ) {
    warning("The argument ServerURL is expected to be NULL or a character.", call. = TRUE)
    return(PRSMonFailureSymbol)
  }

  prsObj$ServerURL <- ServerURL

  prsObj
}

##===========================================================
## LastQuery setter
##===========================================================

#' Set LastQuery.
#' @description Sets LastQuery into the monad object.
#' @param prsObj An PRSMon object.
#' @param LastQuery An object member to be set.
#' @return An PRSMon object.
#' @family Set/Take functions
#' @export
PRSMonSetLastQuery <- function( prsObj, LastQuery ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( !( is.null(LastQuery) || is.character(LastQuery)) ) {
    warning("The argument LastQuery is expected to be NULL or a character.", call. = TRUE)
    return(PRSMonFailureSymbol)
  }

  prsObj$LastQuery <- LastQuery

  prsObj
}

##===========================================================
## LastResult setter
##===========================================================

#' Set LastResult.
#' @description Sets LastResult into the monad object.
#' @param prsObj An PRSMon object.
#' @param LastResult An object member to be set.
#' @return An PRSMon object.
#' @family Set/Take functions
#' @export
PRSMonSetLastResult <- function( prsObj, LastResult ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( !( is.null(LastResult) || is.character(LastResult)) ) {
    warning("The argument LastResult is expected to be NULL or a character.", call. = TRUE)
    return(PRSMonFailureSymbol)
  }

  prsObj$LastResult <- LastResult

  prsObj
}

##===========================================================
## DatabaseURL setter
##===========================================================

#' Set DatabaseURL.
#' @description Sets DatabaseURL into the monad object.
#' @param prsObj An PRSMon object.
#' @param DatabaseURL An object member to be set.
#' @return An PRSMon object.
#' @family Set/Take functions
#' @export
PRSMonSetDatabaseURL <- function( prsObj, DatabaseURL ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( !( is.null(DatabaseURL) || is.character(DatabaseURL)) ) {
    warning("The argument DatabaseURL is expected to be NULL or a character.", call. = TRUE)
    return(PRSMonFailureSymbol)
  }

  prsObj$DatabaseURL <- DatabaseURL

  prsObj
}

##===========================================================
## Parser setter
##===========================================================

#' Set Parser.
#' @description Sets Parser into the monad object.
#' @param prsObj An PRSMon object.
#' @param Parser An object member to be set.
#' @return An PRSMon object.
#' @family Set/Take functions
#' @export
PRSMonSetParser <- function( prsObj, Parser ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( !( is.null(Parser) || is.character(Parser)) ) {
    warning("The argument Parser is expected to be NULL or a character.", call. = TRUE)
    return(PRSMonFailureSymbol)
  }

  prsObj$Parser <- Parser

  prsObj
}

##===========================================================
## ItemColumnName setter
##===========================================================

#' Set ItemColumnName.
#' @description Sets ItemColumnName into the monad object.
#' @param prsObj An PRSMon object.
#' @param ItemColumnName An object member to be set.
#' @return An PRSMon object.
#' @family Set/Take functions
#' @export
PRSMonSetItemColumnName <- function( prsObj, ItemColumnName ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( !( is.null(ItemColumnName) || is.character(ItemColumnName)) ) {
    warning("The argument ItemColumnName is expected to be NULL or a character.", call. = TRUE)
    return(PRSMonFailureSymbol)
  }

  prsObj$ItemColumnName <- ItemColumnName

  prsObj
}

##===========================================================
## ServerSideObjectID Taker
##===========================================================

#' Take ServerSideObjectID.
#' @description Takes ServerSideObjectID from the monad object.
#' @param prsObj An PRSMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{PRSMonFailureSymbol}.
#' @family Set/Take functions
#' @export
PRSMonTakeServerSideObjectID <- function( prsObj, functionName = "PRSMonTakeServerSideObjectID" ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( !PRSMonMemberPresenceCheck( prsObj, memberName = "ServerSideObjectID", memberPrettyName = "ServerSideObjectID", functionName = functionName,  logicalResult = TRUE) ) {
    return(PRSMonFailureSymbol)
  }

  prsObj$ServerSideObjectID
}

##===========================================================
## ServerURL Taker
##===========================================================

#' Take ServerURL.
#' @description Takes ServerURL from the monad object.
#' @param prsObj An PRSMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{PRSMonFailureSymbol}.
#' @family Set/Take functions
#' @export
PRSMonTakeServerURL <- function( prsObj, functionName = "PRSMonTakeServerURL" ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( !PRSMonMemberPresenceCheck( prsObj, memberName = "ServerURL", memberPrettyName = "ServerURL", functionName = functionName,  logicalResult = TRUE) ) {
    return(PRSMonFailureSymbol)
  }

  prsObj$ServerURL
}

##===========================================================
## LastQuery Taker
##===========================================================

#' Take LastQuery.
#' @description Takes LastQuery from the monad object.
#' @param prsObj An PRSMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{PRSMonFailureSymbol}.
#' @family Set/Take functions
#' @export
PRSMonTakeLastQuery <- function( prsObj, functionName = "PRSMonTakeLastQuery" ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( !PRSMonMemberPresenceCheck( prsObj, memberName = "LastQuery", memberPrettyName = "LastQuery", functionName = functionName,  logicalResult = TRUE) ) {
    return(PRSMonFailureSymbol)
  }

  prsObj$LastQuery
}

##===========================================================
## LastResult Taker
##===========================================================

#' Take LastResult.
#' @description Takes LastResult from the monad object.
#' @param prsObj An PRSMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{PRSMonFailureSymbol}.
#' @family Set/Take functions
#' @export
PRSMonTakeLastResult <- function( prsObj, functionName = "PRSMonTakeLastResult" ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( !PRSMonMemberPresenceCheck( prsObj, memberName = "LastResult", memberPrettyName = "LastResult", functionName = functionName,  logicalResult = TRUE) ) {
    return(PRSMonFailureSymbol)
  }

  prsObj$LastResult
}

##===========================================================
## DatabaseURL Taker
##===========================================================

#' Take DatabaseURL.
#' @description Takes DatabaseURL from the monad object.
#' @param prsObj An PRSMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{PRSMonFailureSymbol}.
#' @family Set/Take functions
#' @export
PRSMonTakeDatabaseURL <- function( prsObj, functionName = "PRSMonTakeDatabaseURL" ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( !PRSMonMemberPresenceCheck( prsObj, memberName = "DatabaseURL", memberPrettyName = "DatabaseURL", functionName = functionName,  logicalResult = TRUE) ) {
    return(PRSMonFailureSymbol)
  }

  prsObj$DatabaseURL
}

##===========================================================
## Parser Taker
##===========================================================

#' Take Parser.
#' @description Takes Parser from the monad object.
#' @param prsObj An PRSMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{PRSMonFailureSymbol}.
#' @family Set/Take functions
#' @export
PRSMonTakeParser <- function( prsObj, functionName = "PRSMonTakeParser" ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( !PRSMonMemberPresenceCheck( prsObj, memberName = "Parser", memberPrettyName = "Parser", functionName = functionName,  logicalResult = TRUE) ) {
    return(PRSMonFailureSymbol)
  }

  prsObj$Parser
}

##===========================================================
## ItemColumnName Taker
##===========================================================

#' Take ItemColumnName.
#' @description Takes ItemColumnName from the monad object.
#' @param prsObj An PRSMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{PRSMonFailureSymbol}.
#' @family Set/Take functions
#' @export
PRSMonTakeItemColumnName <- function( prsObj, functionName = "PRSMonTakeItemColumnName" ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( !PRSMonMemberPresenceCheck( prsObj, memberName = "ItemColumnName", memberPrettyName = "ItemColumnName", functionName = functionName,  logicalResult = TRUE) ) {
    return(PRSMonFailureSymbol)
  }

  prsObj$ItemColumnName
}


##===========================================================
## Get history query string
##===========================================================

#' From a history specification into a query string
#' @description Transforms a history specification into a data frame.
#' @param prsObj An PRSMon object.
#' @param history History specification.
#' A data frame with columns \code{c("Rating", "Item")};
#' a numeric vector named elements, the names being items;
#' a character vector, the correspond ratings assumed all to be 1.
#' @param functionName A string that is a name of this function or a delegating function.
#' @param warningQ Should a warning be issued if \code{history} is of unknown type?
#' @details The result data frame is with columns "Score", \code{smr$ItemColumnName};
#' assigned to \code{prsObj$Value}.
#' If \code{history = NULL} then \code{prsObj$Value} is used.
#' @return A PRSMon object
#' @export
PRSMonGetHistoryQueryString <- function( prsObj, history, functionName = "PRSMonGetHistoryDataFrame", warningQ = TRUE ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( is.null(history) ) {
    history <- prsObj %>% PRSMonTakeValue
  }

  if( is.data.frame(history) && sum( c("Rating", "Item", prsObj$ItemColumnName ) %in% names(history) ) == 2 ) {

    names(history) <- gsub( prsObj$ItemColumnName, "Item", names(history) )

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
    return(PRSMonFailureSymbol)

  }

  prsObj$Value <- paste( historyItems, collapse = "," )

  prsObj
}


##===========================================================
## Get profile query string
##===========================================================

#' From a profile specification into query string
#' @description Transforms a profile specification into a data frame.
#' @param prsObj An PRSMon object.
#' @param profile Profile specification.
#' A data frame with columns \code{c("Score", "Tag")};
#' a numeric vector named elements, the names being items;
#' a character vector, the correspond ratings assumed all to be 1.
#' @param functionName A string that is a name of this function or a delegating function.
#' @param warningQ Should a warning be issued if \code{profile} is of unknown type?
#' @details The result is assigned to \code{prsObj$Value}.
#' If \code{profile = NULL} then \code{prsObj$Value} is used.
#' @return A PRSMon object
#' @export
PRSMonGetProfileQueryString <- function( prsObj, profile, functionName = "PRSMonGetProfileDataFrame", warningQ = TRUE ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( is.null(profile) ) {
    profile <- prsObj %>% PRSMonTakeValue
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
    return(PRSMonFailureSymbol)

  }

  prsObj$Value <- paste( profileTags, collapse = "," )

  prsObj
}


##===========================================================
## Recommend by history
##===========================================================

#' Retrieve recommendations
#' @description Recommend items based on history of consumption.
#' @param prsObj An PRSMon object.
#' @param history History specification.
#' A data frame with columns \code{c("Rating", "Item")};
#' a numeric vector named elements, the names being items;
#' a character vector, the correspond ratings assumed all to be 1.
#' @param nrecs Number of recommendations to be returned.
#' @param additionalURLParameters A character vector with named elements each corresponding to
#' an additional parameter to be passed through the constructed query URL.
#' @param parser A string identifying parser to be applied or a parser function.
#' If NULL no parser is applied.
#' @param GET.with A string for the GET command.
#' @details The result is assigned to \code{prsObj$Value}.
#' @return A PRSMon object
#' @family Recommendations computation functions
#' @export
PRSMonRecommend <- function( prsObj, history, nrecs = 12, additionalURLParameters = NULL, parser = "json", GET.with = "recommend" ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( !PRSMonMemberPresenceCheck( prsObj, memberName = "ServerURL", memberPrettyName = "ServerURL", functionName = "PRSMonRecommend",  logicalResultQ = TRUE) ) {
    return(PRSMonFailureSymbol)
  }

  prsObj2 <- prsObj %>% PRSMonGetHistoryQueryString( history = history, functionName = "PRSMonRecommend", warningQ = warningQ )

  if( PRSMonFailureQ(prsObj2) ) { return(PRSMonFailureSymbol) }

  historyQuery <- prsObj2 %>% PRSMonTakeValue

  res <- RCurl::getURL( url = paste0( prsObj$ServerURL, "/", GET.with, "?history=", historyQuery, "&nrecs=", nrecs ) )

  if( is.function(parser) ) {
    res <- parser(res)
  } else if ( is.character(parser) && tolower(parser) %in% tolower( c("json", "fromJSON") ) ) {
    res <- jsonlite::fromJSON( txt = res )
  }

  prsObj$Value <- res

  prsObj
}


##===========================================================
## Recommend By Profile
##===========================================================

#' Retrieve profile recommendations
#' @description Recommend items based on profiles of tags.
#' @param prsObj An PRSMon object.
#' @param profile Profile specification.
#' A data frame with columns \code{c("Score", "Tag")};
#' a numeric vector with named elements, the names being items;
#' a character vector, the correspond ratings assumed all to be 1.
#' @param nrecs Number of recommendations to be returned.
#' @param additionalURLParameters A character vector with named elements each corresponding to
#' an additional parameter to be passed through the constructed query URL.
#' @param parser A string identifying parser to be applied or a parser function.
#' If NULL no parser is applied.
#' @param GET.with A string for the GET command.
#' @details The result is assigned to \code{prsObj$Value}.
#' @return A PRSMon object
#' @family Recommendations computation functions
#' @export
PRSMonRecommendByProfile <- function( prsObj, profile, nrecs = 12, additionalURLParameters = NULL, parser = "json", GET.with = "recommendbyprofile" ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( !PRSMonMemberPresenceCheck( prsObj, memberName = "ServerURL", memberPrettyName = "ServerURL", functionName = "PRSMonRecommendByProfile",  logicalResultQ = TRUE) ) {
    return(PRSMonFailureSymbol)
  }

  prsObj2 <- prsObj %>% PRSMonGetProfileQueryString( profile = profile, functionName = "PRSMonRecommend", warningQ = warningQ )

  if( PRSMonFailureQ(prsObj2) ) { return(PRSMonFailureSymbol) }

  profileQuery <- prsObj2 %>% PRSMonTakeValue

  res <- RCurl::getURL( url = paste0( prsObj$ServerURL, "/", GET.with, "?profile=", profileQuery, "&nrecs=", nrecs ) )

  if( is.function(parser) ) {
    res <- parser(res)
  } else if ( is.character(parser) && tolower(parser) %in% tolower( c("json", "fromJSON") ) ) {
    res <- jsonlite::fromJSON( txt = res )
  }

  prsObj$Value <- res

  prsObj
}


##===========================================================
## Classify By Profile
##===========================================================

#' Retrieve profile classifications
#' @description Classify tag type tags based on profiles of tags.
#' @param prsObj An PRSMon object.
#' @param tagType Tag type for which the classification is done.
#' @param profile Profile specification.
#' A data frame with columns \code{c("Score", "Tag")};
#' a numeric vector with named elements, the names being items;
#' a character vector, the correspond ratings assumed all to be 1.
#' @param nrecs Number of recommendations to be returned.
#' @param additionalURLParameters A character vector with named elements each corresponding to
#' an additional parameter to be passed through the constructed query URL.
#' @param parser A string identifying parser to be applied or a parser function.
#' If NULL no parser is applied.
#' @param GET.with A string for the GET command.
#' @details The result is assigned to \code{prsObj$Value}.
#' @return A PRSMon object
#' @family Recommendations computation functions
#' @export
PRSMonClassifyByProfile <- function( prsObj, tagType, profile, additionalURLParameters = NULL, parser = "json", GET.with = "classifybyprofile" ) {

  if( PRSMonFailureQ(prsObj) ) { return(PRSMonFailureSymbol) }

  if( !PRSMonMemberPresenceCheck( prsObj, memberName = "ServerURL", memberPrettyName = "ServerURL", functionName = "PRSMonClassifyByProfile",  logicalResultQ = TRUE) ) {
    return(PRSMonFailureSymbol)
  }

  prsObj2 <- prsObj %>% PRSMonGetProfileQueryString( profile = profile, functionName = "PRSMonRecommend", warningQ = warningQ )

  if( PRSMonFailureQ(prsObj2) ) { return(PRSMonFailureSymbol) }

  profileQuery <- prsObj2 %>% PRSMonTakeValue

  res <- RCurl::getURL( url = paste0( prsObj$ServerURL, "/", GET.with, "?tagtype=", tagType, "&profile=", profileQuery ) )

  if( is.function(parser) ) {
    res <- parser(res)
  } else if ( is.character(parser) && tolower(parser) %in% tolower( c("json", "fromJSON") ) ) {
    res <- jsonlite::fromJSON( txt = res )
  }

  prsObj$Value <- res

  prsObj
}
