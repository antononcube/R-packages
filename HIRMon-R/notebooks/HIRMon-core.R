
#' @import magrittr
NULL

##===========================================================
## HIRMon failure symbol
##===========================================================

#' Failure symbol for HIRMon.
#' @description Failure symbol for the monad HIRMon.
#' @export
HIRMonFailureSymbol <- NA

#' Failure test for an HIRMon object.
#' @description Test is an HIRMon object a failure symbol.
#' @export
HIRMonFailureQ <- function(x) { mean(is.na(x)) }


##===========================================================
## Failure function
##===========================================================

#' Failure producing function
#' @description Puts the monad object into a monadic failure.
#' @param hirObj An HIRMon object.
#' @param message A message to echoed. If NULL no message is echoed.
#' @return A HIRMon object.
#' @export
HIRMonFailure <- function( hirObj, message = NULL ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  if( is.character(message) ) {
    warning( message, call. = FALSE )
  }

  HIRMonFailureSymbol
}


##===========================================================
## HIRMon Unit
##===========================================================

#' Make a HIRMon Unit
#' @description Creates a monad object.
#' @return An S3 class "HIRMon". In other words, a list with the attribute "class" set to "HIRMon".
#' @export
HIRMonUnit <- function( ) {

  res <- list( Value = NULL, M = NULL, M01 = NULL, TagTypes = NULL, TagTypeRanges = NULL, Data = NULL, HubItemMatrix = NULL, ItemHubMatrix = NULL, BiasVector = NULL )
  attr(res, "class") <- "HIRMon"

  res
}


##===========================================================
## Value setter and getter
##===========================================================

#' Set the value in a HIRMon object.
#' @description Sets the value in a HIRMon monad object.
#' @param hirObj An HIRMon object.
#' @param value The new value.
#' @return A HIRMon object.
#' @details Assigns \code{value} to \code{hirObj$Value}.
#' @family Set/Take functions
#' @export
HIRMonSetValue <- function( hirObj, value ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  hirObj$Value <- value
  hirObj
}

#' Take the value in a HIRMon object.
#' @description Takes the value from HIRMon monad object.
#' @param hirObj An HIRMon object.
#' @return Just \code{hirObj$Value}.
#' @family Set/Take functions
#' @export
HIRMonTakeValue <- function( hirObj ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  hirObj$Value
}


##===========================================================
## Member presence check
##===========================================================

#' General member presence check.
#' @description A general function for checking the presence of a data member in an HIRMon object.
#' @param hirObj An HIRMon object.
#' @param memberName The name of the member to be checked.
#' @param memberPrettyName A pretty member name (for messages).
#' @param functionName The name of the delegating function.
#' @param logicalResultQ Should the result be a logical value?
#' @return A logical value or an HIRMon object.
#' @export
HIRMonMemberPresenceCheck <- function( hirObj, memberName, memberPrettyName = memberName, functionName = "", logicalResultQ = FALSE ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  res <- TRUE

  if( nchar(functionName) > 0 ) { functionName <- paste0( functionName, ":: ") }

  if( is.null(hirObj[[memberName]]) ) {
    warning( paste0( functionName, paste0("Cannot find ", memberPrettyName, ".") ), call. = TRUE )
    res <- FALSE
  }

  if( logicalResultQ ) { res }
  else if ( !logicalResultQ && !res) { HIRMonFailureSymbol }
  else { hirObj }
}


##===========================================================
## Echo monad's value
##===========================================================

#' Echo monad's value.
#' @description Prints the "Value" element/member of the monad object.
#' @param hirObj An HIRMon object.
#' @return A HIRMon object.
#' @details Prints \code{hirObj$Value}.
#' @export
HIRMonEchoValue <- function( hirObj ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  print( hirObj$Value )

  hirObj
}


##===========================================================
## Echo function application of over monad's value
##===========================================================

#' Echo function application to monad's value.
#' @description Applies a function to the "Value" element/member of the monad object
#' and prints the result.
#' @param hirObj An HIRMon object.
#' @param f A function to be applied to \code{hirObj$Value}.
#' @return A HIRMon object.
#' @details Prints \code{f(hirObj$Value)}.
#' @export
HIRMonEchoFunctionValue <- function( hirObj, f ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  print( f(hirObj$Value) )

  hirObj
}


##===========================================================
## Echo
##===========================================================

#' Echo.
#' @description Echoes given argument.
#' @param hirObj An HIRMon object.
#' @param x Object to be echoed.
#' @return An HIRMon object
#' @family Echo functions
#' @export
HIRMonEcho <- function( hirObj, x ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  print(x)

  hirObj
}


##===========================================================
## Optional function application over monad's object
##===========================================================

#' Optional function application to monad's object.
#' @description If monadic failure is obtained from \code{f(hirObj)}
#' then returns the original \code{hirObj};
#' else returns the result of \code{f(hirObj)}.
#' @param hirObj An HIRMon object.
#' @param f A function to be applied to the monad object.
#' @return A HIRMon object.
#' @details In general \code{f} should return a monad object,
#' but that is not enforced.
#' @export
HIRMonOption <- function( hirObj, f ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  res <- hirObj %>% f

  if( HIRMonFailureQ(res) ) { return(hirObj) }

  res
}

##===========================================================
## M setter
##===========================================================

#' Set M.
#' @description Sets M into the monad object.
#' @param hirObj An HIRMon object.
#' @param M An object member to be set.
#' @return An HIRMon object.
#' @family Set/Take functions
#' @export
HIRMonSetM <- function( hirObj, M ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  if( !( is.null(M) || is.matrix(M)) ) {
    warning("The argument M is expected to be NULL or a matrix.", call. = TRUE)
    return(HIRMonFailureSymbol)
  }

  hirObj$M <- M

  hirObj
}

##===========================================================
## M01 setter
##===========================================================

#' Set M01.
#' @description Sets M01 into the monad object.
#' @param hirObj An HIRMon object.
#' @param M01 An object member to be set.
#' @return An HIRMon object.
#' @family Set/Take functions
#' @export
HIRMonSetM01 <- function( hirObj, M01 ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  if( !( is.null(M01) || is.matrix(M01)) ) {
    warning("The argument M01 is expected to be NULL or a matrix.", call. = TRUE)
    return(HIRMonFailureSymbol)
  }

  hirObj$M01 <- M01

  hirObj
}

##===========================================================
## TagTypes setter
##===========================================================

#' Set TagTypes.
#' @description Sets TagTypes into the monad object.
#' @param hirObj An HIRMon object.
#' @param TagTypes An object member to be set.
#' @return An HIRMon object.
#' @family Set/Take functions
#' @export
HIRMonSetTagTypes <- function( hirObj, TagTypes ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  if( !( is.null(TagTypes) || is.character(TagTypes)) ) {
    warning("The argument TagTypes is expected to be NULL or a character.", call. = TRUE)
    return(HIRMonFailureSymbol)
  }

  hirObj$TagTypes <- TagTypes

  hirObj
}

##===========================================================
## TagTypeRanges setter
##===========================================================

#' Set TagTypeRanges.
#' @description Sets TagTypeRanges into the monad object.
#' @param hirObj An HIRMon object.
#' @param TagTypeRanges An object member to be set.
#' @return An HIRMon object.
#' @family Set/Take functions
#' @export
HIRMonSetTagTypeRanges <- function( hirObj, TagTypeRanges ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  if( !( is.null(TagTypeRanges) || is.data.frame(TagTypeRanges)) ) {
    warning("The argument TagTypeRanges is expected to be NULL or a data.frame.", call. = TRUE)
    return(HIRMonFailureSymbol)
  }

  hirObj$TagTypeRanges <- TagTypeRanges

  hirObj
}

##===========================================================
## Data setter
##===========================================================

#' Set Data.
#' @description Sets Data into the monad object.
#' @param hirObj An HIRMon object.
#' @param Data An object member to be set.
#' @return An HIRMon object.
#' @family Set/Take functions
#' @export
HIRMonSetData <- function( hirObj, Data ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  if( !( is.null(Data) || is.data.frame(Data)) ) {
    warning("The argument Data is expected to be NULL or a data.frame.", call. = TRUE)
    return(HIRMonFailureSymbol)
  }

  hirObj$Data <- Data

  hirObj
}

##===========================================================
## HubItemMatrix setter
##===========================================================

#' Set HubItemMatrix.
#' @description Sets HubItemMatrix into the monad object.
#' @param hirObj An HIRMon object.
#' @param HubItemMatrix An object member to be set.
#' @return An HIRMon object.
#' @family Set/Take functions
#' @export
HIRMonSetHubItemMatrix <- function( hirObj, HubItemMatrix ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  if( !( is.null(HubItemMatrix) || is.matrix(HubItemMatrix)) ) {
    warning("The argument HubItemMatrix is expected to be NULL or a matrix.", call. = TRUE)
    return(HIRMonFailureSymbol)
  }

  hirObj$HubItemMatrix <- HubItemMatrix

  hirObj
}

##===========================================================
## ItemHubMatrix setter
##===========================================================

#' Set ItemHubMatrix.
#' @description Sets ItemHubMatrix into the monad object.
#' @param hirObj An HIRMon object.
#' @param ItemHubMatrix An object member to be set.
#' @return An HIRMon object.
#' @family Set/Take functions
#' @export
HIRMonSetItemHubMatrix <- function( hirObj, ItemHubMatrix ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  if( !( is.null(ItemHubMatrix) || is.matrix(ItemHubMatrix)) ) {
    warning("The argument ItemHubMatrix is expected to be NULL or a matrix.", call. = TRUE)
    return(HIRMonFailureSymbol)
  }

  hirObj$ItemHubMatrix <- ItemHubMatrix

  hirObj
}

##===========================================================
## BiasVector setter
##===========================================================

#' Set BiasVector.
#' @description Sets BiasVector into the monad object.
#' @param hirObj An HIRMon object.
#' @param BiasVector An object member to be set.
#' @return An HIRMon object.
#' @family Set/Take functions
#' @export
HIRMonSetBiasVector <- function( hirObj, BiasVector ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  if( !( is.null(BiasVector) || is.numeric(BiasVector)) ) {
    warning("The argument BiasVector is expected to be NULL or a numeric.", call. = TRUE)
    return(HIRMonFailureSymbol)
  }

  hirObj$BiasVector <- BiasVector

  hirObj
}

##===========================================================
## M Taker
##===========================================================

#' Take M.
#' @description Takes M from the monad object.
#' @param hirObj An HIRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{HIRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
HIRMonTakeM <- function( hirObj, functionName = "HIRMonTakeM" ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  if( !HIRMonMemberPresenceCheck( hirObj, memberName = "M", memberPrettyName = "M", functionName = functionName,  logicalResult = TRUE) ) {
    return(HIRMonFailureSymbol)
  }

  hirObj$M
}

##===========================================================
## M01 Taker
##===========================================================

#' Take M01.
#' @description Takes M01 from the monad object.
#' @param hirObj An HIRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{HIRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
HIRMonTakeM01 <- function( hirObj, functionName = "HIRMonTakeM01" ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  if( !HIRMonMemberPresenceCheck( hirObj, memberName = "M01", memberPrettyName = "M01", functionName = functionName,  logicalResult = TRUE) ) {
    return(HIRMonFailureSymbol)
  }

  hirObj$M01
}

##===========================================================
## TagTypes Taker
##===========================================================

#' Take TagTypes.
#' @description Takes TagTypes from the monad object.
#' @param hirObj An HIRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{HIRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
HIRMonTakeTagTypes <- function( hirObj, functionName = "HIRMonTakeTagTypes" ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  if( !HIRMonMemberPresenceCheck( hirObj, memberName = "TagTypes", memberPrettyName = "TagTypes", functionName = functionName,  logicalResult = TRUE) ) {
    return(HIRMonFailureSymbol)
  }

  hirObj$TagTypes
}

##===========================================================
## TagTypeRanges Taker
##===========================================================

#' Take TagTypeRanges.
#' @description Takes TagTypeRanges from the monad object.
#' @param hirObj An HIRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{HIRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
HIRMonTakeTagTypeRanges <- function( hirObj, functionName = "HIRMonTakeTagTypeRanges" ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  if( !HIRMonMemberPresenceCheck( hirObj, memberName = "TagTypeRanges", memberPrettyName = "TagTypeRanges", functionName = functionName,  logicalResult = TRUE) ) {
    return(HIRMonFailureSymbol)
  }

  hirObj$TagTypeRanges
}

##===========================================================
## Data Taker
##===========================================================

#' Take Data.
#' @description Takes Data from the monad object.
#' @param hirObj An HIRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{HIRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
HIRMonTakeData <- function( hirObj, functionName = "HIRMonTakeData" ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  if( !HIRMonMemberPresenceCheck( hirObj, memberName = "Data", memberPrettyName = "Data", functionName = functionName,  logicalResult = TRUE) ) {
    return(HIRMonFailureSymbol)
  }

  hirObj$Data
}

##===========================================================
## HubItemMatrix Taker
##===========================================================

#' Take HubItemMatrix.
#' @description Takes HubItemMatrix from the monad object.
#' @param hirObj An HIRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{HIRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
HIRMonTakeHubItemMatrix <- function( hirObj, functionName = "HIRMonTakeHubItemMatrix" ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  if( !HIRMonMemberPresenceCheck( hirObj, memberName = "HubItemMatrix", memberPrettyName = "HubItemMatrix", functionName = functionName,  logicalResult = TRUE) ) {
    return(HIRMonFailureSymbol)
  }

  hirObj$HubItemMatrix
}

##===========================================================
## ItemHubMatrix Taker
##===========================================================

#' Take ItemHubMatrix.
#' @description Takes ItemHubMatrix from the monad object.
#' @param hirObj An HIRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{HIRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
HIRMonTakeItemHubMatrix <- function( hirObj, functionName = "HIRMonTakeItemHubMatrix" ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  if( !HIRMonMemberPresenceCheck( hirObj, memberName = "ItemHubMatrix", memberPrettyName = "ItemHubMatrix", functionName = functionName,  logicalResult = TRUE) ) {
    return(HIRMonFailureSymbol)
  }

  hirObj$ItemHubMatrix
}

##===========================================================
## BiasVector Taker
##===========================================================

#' Take BiasVector.
#' @description Takes BiasVector from the monad object.
#' @param hirObj An HIRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{HIRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
HIRMonTakeBiasVector <- function( hirObj, functionName = "HIRMonTakeBiasVector" ) {

  if( HIRMonFailureQ(hirObj) ) { return(HIRMonFailureSymbol) }

  if( !HIRMonMemberPresenceCheck( hirObj, memberName = "BiasVector", memberPrettyName = "BiasVector", functionName = functionName,  logicalResult = TRUE) ) {
    return(HIRMonFailureSymbol)
  }

  hirObj$BiasVector
}
