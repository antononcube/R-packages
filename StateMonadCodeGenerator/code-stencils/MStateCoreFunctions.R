
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
## Failure function
##===========================================================

#' Failure producing function
#' @description Puts the monad object into a monadic failure.
#' @param msObj An MState object.
#' @param message A message to echoed. If NULL no message is echoed.
#' @return A MState object.
#' @export
MStateFailure <- function( msObj, message = NULL ) {

  if( MStateFailureQ(msObj) ) { return(MStateFailureSymbol) }

  if( is.character(message) ) {
    warning( message, call. = FALSE )
  }

  MStateFailureSymbol
}


##===========================================================
## MState Unit
##===========================================================

#' Make a MState Unit
#' @description Creates a monad object.
#' @return An S3 class "MState". In other words, a list with the attribute "class" set to "MState".
#' @export
MStateUnit <- function( ) {

  res <- list( Value = NULL )
  attr(res, "class") <- "MState"

  res
}


##===========================================================
## Value setter and getter
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
#' @param logicalResultQ Should the result be a logical value?
#' @return A logical value or an MState object.
#' @export
MStateMemberPresenceCheck <- function( msObj, memberName, memberPrettyName = memberName, functionName = "", logicalResultQ = FALSE ) {

  if( MStateFailureQ(msObj) ) { return(MStateFailureSymbol) }

  res <- TRUE

  if( nchar(functionName) > 0 ) { functionName <- paste0( functionName, ":: ") }

  if( is.null(msObj[[memberName]]) ) {
    warning( paste0( functionName, paste0("Cannot find ", memberPrettyName, ".") ), call. = TRUE )
    res <- FALSE
  }

  if( logicalResultQ ) { res }
  else if ( !logicalResultQ && !res) { MStateFailureSymbol }
  else { msObj }
}


##===========================================================
## Echo monad's value
##===========================================================

#' Echo monad's value.
#' @description Prints the "Value" element/member of the monad object.
#' @param msObj An MState object.
#' @return A MState object.
#' @details Prints \code{msObj$Value}.
#' @export
MStateEchoValue <- function( msObj ) {

  if( MStateFailureQ(msObj) ) { return(MStateFailureSymbol) }

  print( msObj$Value )

  msObj
}


##===========================================================
## Echo function application of over monad's value
##===========================================================

#' Echo function application to monad's value.
#' @description Applies a function to the "Value" element/member of the monad object
#' and prints the result.
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


##===========================================================
## Echo
##===========================================================

#' Echo.
#' @description Echoes given argument.
#' @param msObj An MState object.
#' @param x Object to be echoed.
#' @return An MState object
#' @family Echo functions
#' @export
MStateEcho <- function( msObj, x ) {

  if( MStateFailureQ(msObj) ) { return(MStateFailureSymbol) }

  print(x)

  msObj
}


##===========================================================
## Optional function application over monad's object
##===========================================================

#' Optional function application to monad's object.
#' @description If monadic failure is obtained from \code{f(msObj)}
#' then returns the original \code{msObj};
#' else returns the result of \code{f(msObj)}.
#' @param msObj An MState object.
#' @param f A function to be applied to the monad object.
#' @return A MState object.
#' @details In general \code{f} should return a monad object,
#' but that is not enforced.
#' @export
MStateOption <- function( msObj, f ) {

  if( MStateFailureQ(msObj) ) { return(MStateFailureSymbol) }

  res <- msObj %>% f

  if( MStateFailureQ(res) ) { return(msObj) }

  res
}
