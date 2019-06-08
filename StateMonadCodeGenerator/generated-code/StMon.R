
#' @import magrittr
NULL

##===========================================================
## StMon failure symbol
##===========================================================

#' Failure symbol for StMon.
#' @description Failure symbol for the monad StMon.
#' @export
StMonFailureSymbol <- NA

#' Failure test for an StMon object.
#' @description Test is an StMon object a failure symbol.
#' @export
StMonFailureQ <- function(x) { mean(is.na(x)) }


##===========================================================
## StMon Unit
##===========================================================

#' Make a StMon Unit
#' @description Creates a monad object.
#' @return An S3 class "StMon". In other words, a list with the attribute "class" set to "StMon".
#' @export
StMonUnit <- function( ) {

  res <- list( Value = NULL, Data = NULL, RegressionFunctions = NULL, Outliers = NULL )
  attr(res, "class") <- "StMon"

  res
}


##===========================================================
## Value setter and getter
##===========================================================

#' Set the value in a StMon object.
#' @description Sets the value in a StMon monad object.
#' @param qrObj An StMon object.
#' @param value The new value.
#' @return A StMon object.
#' @details Assigns \code{value} to \code{qrObj$Value}.
#' @family Set/Take functions
#' @export
StMonSetValue <- function( qrObj, value ) {

  if( StMonFailureQ(qrObj) ) { return(StMonFailureSymbol) }

  qrObj$Value <- value
  qrObj
}

#' Take the value in a StMon object.
#' @description Takes the value from StMon monad object.
#' @param qrObj An StMon object.
#' @return Just \code{qrObj$Value}.
#' @family Set/Take functions
#' @export
StMonTakeValue <- function( qrObj ) {

  if( StMonFailureQ(qrObj) ) { return(StMonFailureSymbol) }

  qrObj$Value
}


##===========================================================
## Member presence check
##===========================================================

#' General member presence check.
#' @description A general function for checking the presence of a data member in an StMon object.
#' @param qrObj An StMon object.
#' @param memberName The name of the member to be checked.
#' @param memberPrettyName A pretty member name (for messages).
#' @param functionName The name of the delegating function.
#' @param logicalResultQ Should the result be a logical value?
#' @return A logical value or an StMon object.
#' @export
StMonMemberPresenceCheck <- function( qrObj, memberName, memberPrettyName = memberName, functionName = "", logicalResultQ = FALSE ) {

  if( StMonFailureQ(qrObj) ) { return(StMonFailureSymbol) }

  res <- TRUE

  if( nchar(functionName) > 0 ) { functionName <- paste0( functionName, ":: ") }

  if( is.null(qrObj[[memberName]]) ) {
    warning( paste0( functionName, paste0("Cannot find ", memberPrettyName, ".") ), call. = TRUE )
    res <- FALSE
  }

  if( logicalResultQ ) { res }
  else if ( !logicalResultQ && !res) { StMonFailureSymbol }
  else { qrObj }
}


##===========================================================
## Echo monad's value
##===========================================================

#' Echo monad's value.
#' @description Prints the "Value" element/member of the monad object.
#' @param qrObj An StMon object.
#' @return A StMon object.
#' @details Prints \code{f(qrObj$Value)}.
#' @export
StMonEchoValue <- function( qrObj ) {

  if( StMonFailureQ(qrObj) ) { return(StMonFailureSymbol) }

  print( qrObj$Value )

  qrObj
}


##===========================================================
## Echo function application of over monad's value
##===========================================================

#' Echo function application to monad's value.
#' @description Applies a function to the "Value" element/member of the monad object
#' and prints the result.
#' @param qrObj An StMon object.
#' @param f A function to be applied to \code{qrObj$Value}.
#' @return A StMon object.
#' @details Prints \code{f(qrObj$Value)}.
#' @export
StMonEchoFunctionValue <- function( qrObj, f ) {

  if( StMonFailureQ(qrObj) ) { return(StMonFailureSymbol) }

  print( f(qrObj$Value) )

  qrObj
}


##===========================================================
## Optional function application over monad's object
##===========================================================

#' Optional function application to monad's object.
#' @description If monadic failure is obtained from \code{qrObj %>% f}
#' then returns the original \code{qrObj};
#' else returns the result of \code{qrObj %>% f}.
#' @param qrObj An StMon object.
#' @param f A function to be applied to the monad object.
#' @return A StMon object.
#' @details In general \code{f} should return a monad object,
#' but that is not enforced.
#' @export
StMonOption <- function( qrObj, f ) {

  if( StMonFailureQ(qrObj) ) { return(StMonFailureSymbol) }

  res <- qrObj %>% f

  if( StMonFailureQ(res) ) { return(qrObj) }

  res
}

##===========================================================
## Data setter
##===========================================================

#' Set Data.
#' @description Sets Data into the monad object.
#' @param qrObj An StMon object.
#' @param Data An object member to be set.
#' @return An StMon object.
#' @family Set/Take functions
#' @export
StMonSetData <- function( qrObj, Data ) {

  if( StMonFailureQ(qrObj) ) { return(StMonFailureSymbol) }

  if( !( is.null(Data) || is.numeric(Data)) ) {
    warning("The argument Data is expected to be NULL or a numeric.", call. = TRUE)
    return(StMonFailureSymbol)
  }

  qrObj$Data <- Data

  qrObj
}

##===========================================================
## RegressionFunctions setter
##===========================================================

#' Set RegressionFunctions.
#' @description Sets RegressionFunctions into the monad object.
#' @param qrObj An StMon object.
#' @param RegressionFunctions An object member to be set.
#' @return An StMon object.
#' @family Set/Take functions
#' @export
StMonSetRegressionFunctions <- function( qrObj, RegressionFunctions ) {

  if( StMonFailureQ(qrObj) ) { return(StMonFailureSymbol) }

  if( !( is.null(RegressionFunctions) || is.list(RegressionFunctions)) ) {
    warning("The argument RegressionFunctions is expected to be NULL or a list.", call. = TRUE)
    return(StMonFailureSymbol)
  }

  qrObj$RegressionFunctions <- RegressionFunctions

  qrObj
}

##===========================================================
## Outliers setter
##===========================================================

#' Set Outliers.
#' @description Sets Outliers into the monad object.
#' @param qrObj An StMon object.
#' @param Outliers An object member to be set.
#' @return An StMon object.
#' @family Set/Take functions
#' @export
StMonSetOutliers <- function( qrObj, Outliers ) {

  if( StMonFailureQ(qrObj) ) { return(StMonFailureSymbol) }

  if( !( is.null(Outliers) || is.list(Outliers)) ) {
    warning("The argument Outliers is expected to be NULL or a list.", call. = TRUE)
    return(StMonFailureSymbol)
  }

  qrObj$Outliers <- Outliers

  qrObj
}

##===========================================================
## Data Taker
##===========================================================

#' Take Data.
#' @description Takes Data from the monad object.
#' @param qrObj An StMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{StMonFailureSymbol}.
#' @family Set/Take functions
#' @export
StMonTakeData <- function( qrObj, functionName = "StMonTakeData" ) {

  if( StMonFailureQ(qrObj) ) { return(StMonFailureSymbol) }

  if( !StMonMemberPresenceCheck( qrObj, memberName = "Data", memberPrettyName = "Data", functionName = functionName,  logicalResult = TRUE) ) {
    return(StMonFailureSymbol)
  }

  qrObj$Data
}

##===========================================================
## RegressionFunctions Taker
##===========================================================

#' Take RegressionFunctions.
#' @description Takes RegressionFunctions from the monad object.
#' @param qrObj An StMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{StMonFailureSymbol}.
#' @family Set/Take functions
#' @export
StMonTakeRegressionFunctions <- function( qrObj, functionName = "StMonTakeRegressionFunctions" ) {

  if( StMonFailureQ(qrObj) ) { return(StMonFailureSymbol) }

  if( !StMonMemberPresenceCheck( qrObj, memberName = "RegressionFunctions", memberPrettyName = "RegressionFunctions", functionName = functionName,  logicalResult = TRUE) ) {
    return(StMonFailureSymbol)
  }

  qrObj$RegressionFunctions
}

##===========================================================
## Outliers Taker
##===========================================================

#' Take Outliers.
#' @description Takes Outliers from the monad object.
#' @param qrObj An StMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{StMonFailureSymbol}.
#' @family Set/Take functions
#' @export
StMonTakeOutliers <- function( qrObj, functionName = "StMonTakeOutliers" ) {

  if( StMonFailureQ(qrObj) ) { return(StMonFailureSymbol) }

  if( !StMonMemberPresenceCheck( qrObj, memberName = "Outliers", memberPrettyName = "Outliers", functionName = functionName,  logicalResult = TRUE) ) {
    return(StMonFailureSymbol)
  }

  qrObj$Outliers
}
