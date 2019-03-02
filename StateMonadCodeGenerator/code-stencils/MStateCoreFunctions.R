
#' @import magrittr
NULL

##===========================================================
## MState failure symbol
##===========================================================

#' Failure symbol for MState.
#' @description Failure symbol for the monad MState.
#' @export
MStateFailureSymbol <- NA

#' Failure test for an MState object.
#' @description Test is an MState object a failure symbol.
#' @export
MStateFailureQ <- function(x) { mean(is.na(x)) }


##===========================================================
## MState Unit
##===========================================================

#' Make a MState Unit
#' @description Creates a monad object.
#' @param data A vector or a two-column matrix or data frame.
#' @return An S3 class "MState". In other words, a list with the attribute "class" set to "MState".
#' @export
MStateUnit <- function( ) {

  res <- list( Value = NULL )
  attr(res, "class") <- "MState"

  res
}


##===========================================================
## Setters and getters
##===========================================================

#' Set the value in a MState object.
#' @description Sets the value in a MState monad object.
#' @param msObj An MState object.
#' @param value The new value.
#' @return A MState object.
#' @details Assigns \code{value} to \code{msObj$Value}.
#' @family Set/Take functions
#' @export
MStateSetValue <- function( msObj, value ) {

  if( MStateFailureQ(msObj) ) { return(MStateFailureSymbol) }

  msObj$Value <- value
  msObj
}

#' Take the value in a MState object.
#' @description Takes the value from MState monad object.
#' @param msObj An MState object.
#' @return Just \code{msObj$Value}.
#' @family Set/Take functions
#' @export
MStateTakeValue <- function( msObj ) {

  if( MStateFailureQ(msObj) ) { return(MStateFailureSymbol) }

  msObj$Value
}


##===========================================================
## Member presence check
##===========================================================

#' General member presence check.
#' @description A general function for checking the presence of a data member in an MState object.
#' @param msObj An MState object.
#' @param memberName The name of the member to be checked.
#' @param memberPrettyName A pretty member name (for messages).
#' @param functionName The name of the delegating function.
#' @param logicalResult Should the result be logical value?
#' @return A logical value or an MState object.
#' @export
MStateMemberPresenceCheck <- function( msObj, memberName, memberPrettyName = memberName, functionName = "", logicalResult = FALSE ) {

  if( MStateFailureQ(msObj) ) { return(MStateFailureSymbol) }

  res <- TRUE

  if( nchar(functionName) > 0 ) { functionName <- paste0( functionName, ":: ") }

  if( is.null(msObj[[memberName]]) ) {
    warning( paste0( functionName, paste0("Cannot find ", memberPrettyName, ".") ), call. = TRUE )
    res <- FALSE
  }

  if( logicalResult ) { res }
  else if ( !logicalResult && !res) { MStateFailureSymbol }
  else { msObj }
}


##===========================================================
## Echo function application of over monad's value
##===========================================================

#' Function application to monad's value.
#' @description Apply a function to the "Value" element/member of the monad object.
#' @param msObj An MState object.
#' @param f A function to be applied to \code{msObj$Value}.
#' @return A MState object.
#' @details Prints \code{f(msObj$Value)}.
#' @export
MStateEchoFunctionValue <- function( msObj, f ) {

  if( MStateFailureQ(msObj) ) { return(MStateFailureSymbol) }

  print( f(msObj$Value) )

  msObj
}


